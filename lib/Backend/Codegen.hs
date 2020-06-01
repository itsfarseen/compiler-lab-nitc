{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ConstraintKinds #-}

module Backend.Codegen where

import Backend.Instructions
import Backend.Reg
import Grammar

parseProgram :: (CompilerClass m) => Program -> m ()
parseProgram program = do
  let (Program stmts) = program
  mapM_ execStmt stmts

execStmt :: (CompilerClass m) => Stmt -> m ()
execStmt stmt = case stmt of
  StmtAssign   stmt -> execStmtAssign stmt
  StmtRead     stmt -> execStmtRead stmt
  StmtWrite    stmt -> execStmtWrite stmt
  StmtIf       stmt -> execStmtIf stmt
  StmtIfElse   stmt -> execStmtIfElse stmt
  StmtWhile    stmt -> execStmtWhile stmt
  StmtBreak    stmt -> execStmtBreak stmt
  StmtContinue stmt -> execStmtContinue stmt

execStmtAssign :: (CompilerClass m) => StmtAssign -> m ()
execStmtAssign stmt = do
  let (MkStmtAssign lhs rhs) = stmt
  rhsReg    <- getRValueInReg rhs
  lhsLocReg <- getLValueLocInReg lhs
  appendCode [XSM_MOV_IndDst lhsLocReg rhsReg]
  releaseReg lhsLocReg
  releaseReg rhsReg
  return ()

execStmtRead :: (CompilerClass m) => StmtRead -> m ()
execStmtRead stmt = do
  let MkStmtRead lValue = stmt
  lValueLocReg <- getLValueLocInReg lValue
  t1           <- getFreeReg
  let code =
        [ XSM_MOV_Int t1 7 -- arg1: Call Number (Read = 7)
        , XSM_PUSH t1
        , XSM_MOV_Int t1 (-1) -- arg2: File Pointer (Stdin = -1)
        , XSM_PUSH t1
        , XSM_PUSH lValueLocReg -- arg3: Buffer loc
        , XSM_PUSH R0 -- arg4: unused
        , XSM_PUSH R0 -- arg5: unused
        , XSM_INT 6 -- Int 6 = Read System Call
        , XSM_POP t1 -- arg5
        , XSM_POP t1 -- arg4
        , XSM_POP t1 -- arg3
        , XSM_POP t1 -- arg2
        , XSM_POP t1 -- arg1
        ]
  appendCode code
  releaseReg t1
  releaseReg lValueLocReg

execStmtWrite :: (CompilerClass m) => StmtWrite -> m ()
execStmtWrite stmt = do
  let MkStmtWrite rValue = stmt
  reg <- getRValueInReg rValue
  printReg reg
  releaseReg reg

execStmtIf :: (CompilerClass m) => StmtIf -> m ()
execStmtIf stmt = do
  let MkStmtIf condition stmts = stmt
  condReg  <- getRValueInReg condition
  endLabel <- getNewLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JZ condReg endLabel]
  mapM_ execStmt stmts
  installLabel endLabel
  releaseReg condReg

execStmtIfElse :: (CompilerClass m) => StmtIfElse -> m ()
execStmtIfElse stmt = do
  let MkStmtIfElse condition stmtsThen stmtsElse = stmt
  condReg   <- getRValueInReg condition
  elseLabel <- getNewLabel
  endLabel  <- getNewLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JZ condReg elseLabel]
  mapM_ execStmt stmtsThen
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]
  installLabel elseLabel
  mapM_ execStmt stmtsElse
  installLabel endLabel
  releaseReg condReg

loopBody :: (CompilerClass m) => (String -> String -> m ()) -> m ()
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

execStmtWhile :: (CompilerClass m) => StmtWhile -> m ()
execStmtWhile stmt = do
  let MkStmtWhile condition stmts = stmt
  loopBody $ \startLabel endLabel -> do
    r <- getRValueInReg condition
    appendCode [XSM_UTJ $ XSM_UTJ_JZ r endLabel]
    releaseReg r
    mapM_ execStmt stmts
    appendCode [XSM_UTJ $ XSM_UTJ_JMP startLabel]

execStmtBreak :: (CompilerClass m) => StmtBreak -> m ()
execStmtBreak _ = do
  endLabel <- peekLoopBreakLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]

execStmtContinue :: (CompilerClass m) => StmtContinue -> m ()
execStmtContinue _ = do
  endLabel <- peekLoopContinueLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]

printReg :: (CompilerClass m) => Reg -> m ()
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
  appendCode code
  releaseReg t1

getLValueLocInReg :: (CompilerClass m) => LValue -> m Reg
getLValueLocInReg lValue = do
  let (LValue indices ident) = lValue
  (DataType dims _) <- getIdentDataType ident
  (reg, _)          <- getLValueLocInReg' dims indices ident
  return reg
 where
  getLValueLocInReg'
    :: (CompilerClass m) => [Int] -> [RValue] -> String -> m (Reg, Int)
  getLValueLocInReg' dims indices identName = case (dims, indices) of
    ([], []) -> do
      reg <- getFreeReg
      loc <- getIdentLocInStack identName
      appendCode [XSM_MOV_Int reg loc]
      return (reg, 1)
    ([], _ : _) -> error $ "Compiler bug: Too many indices "
    (_ : _, []) -> error $ "Compiler bug: Too less indices: " ++ (show dims)
    (d : ds, i : is) -> do
      (reg, innerSize) <- getLValueLocInReg' ds is identName
      rhs              <- getRValueInReg i
      appendCode [XSM_MUL_I rhs innerSize]
      appendCode [XSM_ADD reg rhs]
      releaseReg rhs
      return (reg, innerSize * d)

getRValueInReg :: (CompilerClass m) => RValue -> m Reg
getRValueInReg rValue = case rValue of
  RExp (ExpNum i) -> do
    reg <- getFreeReg
    appendCode [XSM_MOV_Int reg i]
    return reg
  RExp (ExpStr s) -> do
    reg <- getFreeReg
    appendCode [XSM_MOV_Str reg s]
    return reg
  RExp    (MkExpArithmetic e1 op e2) -> execALUInstr (arithOpInstr op) e1 e2
  RExp    (MkExpLogical    e1 op e2) -> execALUInstr (logicOpInstr op) e1 e2
  RLValue lValue                     -> do
    reg <- getLValueLocInReg lValue
    appendCode [XSM_MOV_IndSrc reg reg]
    return reg

type ALUInstr = (Reg -> Reg -> XSMInstr)

execALUInstr :: (CompilerClass m) => ALUInstr -> RValue -> RValue -> m Reg
execALUInstr instr e1 e2 = do
  r1 <- getRValueInReg e1
  r2 <- getRValueInReg e2
  appendCode [instr r1 r2]
  releaseReg r2
  return r1

arithOpInstr :: OpArithmetic -> ALUInstr
arithOpInstr op = case op of
  OpAdd -> XSM_ADD
  OpSub -> XSM_SUB
  OpMul -> XSM_MUL
  OpDiv -> XSM_DIV
  OpMod -> XSM_MOD

logicOpInstr :: OpLogical -> ALUInstr
logicOpInstr op = case op of
  OpLT -> XSM_LT
  OpGT -> XSM_GT
  OpLE -> XSM_LE
  OpGE -> XSM_GE
  OpNE -> XSM_NE
  OpEQ -> XSM_EQ


class Monad m => Idents m where
  getIdentLocInStack :: String -> m Int
  getIdentDataType :: String -> m DataType

class Monad m => FreeRegs m where
  getFreeReg :: m Reg
  releaseReg :: Reg -> m ()

class Monad m => Code m where
  appendCode :: [XSMInstr] -> m ()

class Code m => Labels m where
  getNewLabel :: m String
  installLabel :: String -> m ()
  pushLoopBreakLabel :: String -> m ()
  pushLoopContinueLabel :: String -> m ()
  popLoopBreakLabel :: m ()
  popLoopContinueLabel :: m ()
  peekLoopBreakLabel :: m String
  peekLoopContinueLabel :: m String

type CompilerClass m = (Idents m, FreeRegs m, Code m, Labels m)
