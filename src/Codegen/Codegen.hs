{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}

module Codegen.Codegen where

import Codegen.Compiler
import Codegen.Instructions
import Codegen.Reg
import Control.Monad.Except
import Control.Monad.State.Strict (gets)
import Data.List
import Grammar
import qualified SymbolTable

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
  let (MkStmtAssign ident exp _) = stmt
  expReg <- parseExp exp
  moveToIdent ident expReg
  releaseReg expReg
  return ()

execStmtRead :: StmtRead -> Compiler ()
execStmtRead stmt = do
  let MkStmtRead ident _ = stmt
  loc <- getIdentLocInStack ident
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
  reg <- parseExp exp
  printReg reg

execStmtIf :: StmtIf -> Compiler ()
execStmtIf stmt = do
  let MkStmtIf expBool stmts _ = stmt
  r        <- parseExp expBool
  endLabel <- getNewLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JZ r endLabel]
  mapM_ execStmt stmts
  installLabel endLabel

execStmtIfElse :: StmtIfElse -> Compiler ()
execStmtIfElse stmt = do
  let MkStmtIfElse expBool stmtsThen stmtsElse _ = stmt
  r         <- parseExp expBool
  elseLabel <- getNewLabel
  endLabel  <- getNewLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JZ r elseLabel]
  mapM_ execStmt stmtsThen
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]
  installLabel elseLabel
  mapM_ execStmt stmtsElse
  installLabel endLabel

execStmtWhile :: StmtWhile -> Compiler ()
execStmtWhile stmt = do
  let MkStmtWhile expBool stmts _ = stmt
  startLabel <- getNewLabel
  endLabel   <- getNewLabel
  pushLoopContinueLabel startLabel
  pushLoopBreakLabel endLabel
  installLabel startLabel
  r <- parseExp expBool
  appendCode [XSM_UTJ $ XSM_UTJ_JZ r endLabel]
  mapM_ execStmt stmts
  appendCode [XSM_UTJ $ XSM_UTJ_JMP startLabel]
  installLabel endLabel
  _ <- popLoopContinueLabel
  _ <- popLoopBreakLabel
  return ()

execStmtDoWhile :: StmtDoWhile -> Compiler ()
execStmtDoWhile stmt = do
  let MkStmtDoWhile expBool stmts _ = stmt
  startLabel <- getNewLabel
  endLabel   <- getNewLabel
  pushLoopContinueLabel startLabel
  pushLoopBreakLabel endLabel
  installLabel startLabel
  mapM_ execStmt stmts
  r <- parseExp expBool
  appendCode [XSM_UTJ $ XSM_UTJ_JZ r endLabel]
  appendCode [XSM_UTJ $ XSM_UTJ_JMP startLabel]
  installLabel endLabel
  _ <- popLoopContinueLabel
  _ <- popLoopBreakLabel
  return ()

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

getIdentInReg :: String -> Compiler Reg
getIdentInReg ident = do
  loc <- getIdentLocInStack ident
  r   <- getFreeReg
  appendCode [XSM_MOV_DirSrc r loc]
  return r

moveToIdent :: String -> Reg -> Compiler ()
moveToIdent ident reg = do
  loc <- getIdentLocInStack ident
  appendCode [XSM_MOV_DirDst loc reg]
  return ()

parseExp exp = case exp of
  (ExpPure (ExpNum d _)) -> do
    r <- getFreeReg
    appendCode [XSM_MOV_Int r d]
    return r
  (ExpPure  (ExpArithmetic e1 op e2 _)) -> execALUInstr (arithOpInstr op) e1 e2
  (ExpPure  (ExpLogical    e1 op e2 _)) -> execALUInstr (logicOpInstr op) e1 e2
  (ExpIdent (MkExpIdent ident _      )) -> getIdentInReg ident

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
