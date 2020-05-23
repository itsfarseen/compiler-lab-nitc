{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Backend.Codegen where

import Backend.Compiler
import Backend.CompilerUtils
import Backend.Instructions
import Backend.Reg
import Control.Monad.Except
import Control.Monad.State.Strict (gets)
import Data.List
import Grammar
import qualified SymbolTable
import qualified Symbol

parseProgram :: Program -> Compiler ()
parseProgram program = do
  let (Program stmts) = program
  mapM_ execStmt stmts

execStmt :: Stmt -> Compiler ()
execStmt stmt = case stmt of
  StmtDeclare  stmt -> execStmtDeclare stmt
  StmtAssign   stmt -> execStmtAssign stmt
  StmtRead     stmt -> execStmtRead stmt
  StmtWrite    stmt -> execStmtWrite stmt
  StmtIf       stmt -> execStmtIf stmt
  StmtIfElse   stmt -> execStmtIfElse stmt
  StmtWhile    stmt -> execStmtWhile stmt
  StmtDoWhile  stmt -> execStmtDoWhile stmt
  StmtBreak    stmt -> execStmtBreak stmt
  StmtContinue stmt -> execStmtContinue stmt

execStmtDeclare :: StmtDeclare -> Compiler ()
execStmtDeclare _ = return ()

execStmtAssign :: StmtAssign -> Compiler ()
execStmtAssign stmt = do
  let (MkStmtAssign lValue rValue _) = stmt
  rhsReg <- calcRValue rValue
  storeLValue lValue rhsReg
  releaseReg rhsReg
  return ()

execStmtRead :: StmtRead -> Compiler ()
execStmtRead stmt = do
  let MkStmtRead lValue _ = stmt
  loc <- getLValueLocInStack lValue
  t1  <- getFreeReg
  let code =
        [ XSM_MOV_Int t1 7 -- arg1: Call Number (Read = 7)
        , XSM_PUSH t1
        , XSM_MOV_Int t1 (-1) -- arg2: File Pointer (Stdin = -1)
        , XSM_PUSH t1
        , XSM_MOV_Int t1 loc -- arg3: Buffer loc
        , XSM_PUSH t1
        , XSM_PUSH R0 -- arg4: unused
        , XSM_PUSH R0 -- arg5: unused
        , XSM_INT 6 -- Int 6 = Read System Call
        , XSM_POP t1 -- arg5
        , XSM_POP t1 -- arg4
        , XSM_POP t1 -- arg3
        , XSM_POP t1 -- arg2
        , XSM_POP t1 -- arg1
        ]
  releaseReg t1
  appendCode code

execStmtWrite :: StmtWrite -> Compiler ()
execStmtWrite stmt = do
  let MkStmtWrite exp _ = stmt
  reg <- calcRValue rValue
  printReg reg

execStmtIf :: StmtIf -> Compiler ()
execStmtIf stmt = do
  let MkStmtIf condition stmts _ = stmt
  r        <- calcRValue condition
  endLabel <- getNewLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JZ r endLabel]
  mapM_ execStmt stmts
  installLabel endLabel

execStmtIfElse :: StmtIfElse -> Compiler ()
execStmtIfElse stmt = do
  let MkStmtIfElse condition stmtsThen stmtsElse _ = stmt
  r         <- calcRValue condition
  elseLabel <- getNewLabel
  endLabel  <- getNewLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JZ r elseLabel]
  mapM_ execStmt stmtsThen
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]
  installLabel elseLabel
  mapM_ execStmt stmtsElse
  installLabel endLabel

loopBody :: (String -> String -> Compiler ()) -> Compiler ()
loopBody body = do
  startLabel <- getNewLabel
  endLabel   <- getNewLabel
  pushLoopContinueLabel startLabel
  pushLoopBreakLabel endLabel
  installLabel startLabel
  body startLabel endLabel
  installLabel endLabel
  _ <- popLoopContinueLabel
  _ <- popLoopBreakLabel
  return ()

execStmtWhile :: StmtWhile -> Compiler ()
execStmtWhile stmt = do
  let MkStmtWhile expBool stmts _ = stmt
  loopBody $ \startLabel endLabel -> do
    r <- calcRValue rValueBool
    appendCode [XSM_UTJ $ XSM_UTJ_JZ r endLabel]
    mapM_ execStmt stmts
    appendCode [XSM_UTJ $ XSM_UTJ_JMP startLabel]

execStmtDoWhile :: StmtDoWhile -> Compiler ()
execStmtDoWhile stmt = do
  let MkStmtDoWhile expBool stmts _ = stmt
  loopBody $ \startLabel endLabel -> do
    mapM_ execStmt stmts
    r <- calcRValue rValueBool
    appendCode [XSM_UTJ $ XSM_UTJ_JZ r endLabel]
    appendCode [XSM_UTJ $ XSM_UTJ_JMP startLabel]

execStmtBreak :: StmtBreak -> Compiler ()
execStmtBreak _ = do
  endLabel <- peekLoopBreakLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]

execStmtContinue :: StmtContinue -> Compiler ()
execStmtContinue _ = do
  endLabel <- peekLoopContinueLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]

printReg :: Reg -> Compiler ()
printReg reg = do
  t1 <- getFreeReg
  let code =
        [ XSM_MOV_Int t1 5 -- arg1: Call Number (Write = 5)
        , XSM_PUSH t1
        , XSM_MOV_Int t1 (-2) -- arg2: File Pointer (Stdout = -2)
        , XSM_PUSH t1
        , XSM_PUSH reg -- arg3: data to be written
        , XSM_PUSH R0 -- arg4: unused
        , XSM_PUSH R0 -- arg5: unused
        , XSM_INT 7 -- Int 7 = Write System Call
        , XSM_POP t1 -- arg5
        , XSM_POP t1 -- arg4
        , XSM_POP t1 -- arg3
        , XSM_POP t1 -- arg2
        , XSM_POP t1 -- arg1
        ]
  releaseReg t1
  appendCode code

type ALUInstr = (Reg -> Reg -> XSMInstr)

execALUInstr :: ALUInstr -> Exp -> Exp -> Compiler Reg
execALUInstr instr e1 e2 = do
  r1 <- parseExp e1
  r2 <- parseExp e2
  appendCode [instr r1 r2]
  releaseReg r2
  return r1

getLValueInReg lValue = case lValue of
  LValueIdent ident -> do
    reg      <- getIdentInReg ident
    dataType <- getIdentDataType ident
    return (reg, dataType)
  LValueArrayIndex (MkArrayIndex lValue index _) -> do
    (reg, dataType) <- getLValueInReg lValue
    let (Symbol.DataTypeArray size innerType) = dataType
    indexReg <- getRValueInReg index
    let innerTypeSize = Symbol.getSize innerType
    t <- getFreeReg
    appendCode [XSM_MOV_Int t innerTypeSize]
    appendCode [XSM_MUL indexReg t]
    return ()

getIdentInReg :: Ident -> Compiler Reg
getIdentInReg ident = do
  loc <- getIdentLocInStack ident
  r   <- getFreeReg
  appendCode [XSM_MOV_DirSrc r loc]
  return r

getIdentIndexInReg :: Ident -> Reg -> Compiler Reg
getIdentIndexInReg ident indexReg = do
  loc <- getIdentLocInStack ident
  r   <- getFreeReg
  appendCode [XSM_MOV_Int r loc]
  appendCode [XSM_ADD r indexReg]
  appendCode [XSM_MOV_IndSrc r r]
  return r

moveToIdent :: Ident -> Reg -> Compiler ()
moveToIdent ident reg = do
  loc <- getIdentLocInStack ident
  appendCode [XSM_MOV_DirDst loc reg]
  return ()

parseExp :: Exp -> Compiler Reg
calcRValue rValue = case exp of
  (ExpPure (ExpNum d _)) -> do
    r <- getFreeReg
    appendCode [XSM_MOV_Int r d]
    return r
  (ExpPure (ExpArithmetic e1 op e2 _)) -> execALUInstr (arithOpInstr op) e1 e2
  (ExpPure (ExpLogical e1 op e2 _)) -> execALUInstr (logicOpInstr op) e1 e2
  (ExpIdent (MkExpIdent ident)) -> getIdentInReg ident
  (ExpArrayIndex (MkExpArrayIndex ident index _)) -> do
    indexReg <- parseExp index
    getIdentIndexInReg ident indexReg

arithOpInstr :: OpArithmetic -> (Reg -> Reg -> XSMInstr)
arithOpInstr op = case op of
  OpAdd -> XSM_ADD
  OpSub -> XSM_SUB
  OpMul -> XSM_MUL
  OpDiv -> XSM_DIV
  OpMod -> XSM_MOD

logicOpInstr :: OpLogical -> (Reg -> Reg -> XSMInstr)
logicOpInstr op = case op of
  OpLT -> XSM_LT
  OpGT -> XSM_GT
  OpLE -> XSM_LE
  OpGE -> XSM_GE
  OpNE -> XSM_NE
  OpEQ -> XSM_EQ
