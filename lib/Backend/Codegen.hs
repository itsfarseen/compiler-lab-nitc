{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}

module Backend.Codegen where

import Backend.Instructions
import Backend.Reg
import Data.List (sortOn)
import qualified Grammar
import Grammar hiding (Symbol(..))
import qualified Data.HashMap.Strict as HM
import Control.Monad.State.Strict
import Error
import Span
import Control.Monad.Except hiding (Error)
import Data.Maybe (fromJust)
import Data.List (find)

type Codegen = StateT CodegenState (Either Error)

data CodegenState =
  CodegenState
    { freeRegs :: [Reg]
    , code :: [XSMInstr]
    , labels :: HM.HashMap String Int
    , lastLabelNo :: Int
    , loopBreakLabels :: [String]
    , loopContinueLabels :: [String]
    , symbols :: [Symbol]
    , symbolsSize :: Int
    }

data Symbol =
  Symbol
    {
      symName :: String
    , symDataType :: Grammar.DataType
    , symRelLoc :: Int
    }

-- data Func = Func {
--     funcName :: String
--   , funcBody :: [Grammar.Stmt]
--   , funcArgs :: [Symbol]
--   , funcLocalVars :: [Symbol]
-- }

runCodegen :: Codegen a -> [Grammar.Symbol] -> Either Error a
runCodegen compiler symbols =
  evalStateT compiler (initCodegenState symbols)


initCodegenState :: [Grammar.Symbol] -> CodegenState
initCodegenState symbols = CodegenState
  { freeRegs           = [R0 .. R19]
  , code               = []
  , labels             = HM.empty
  , lastLabelNo        = 0
  , loopBreakLabels    = []
  , loopContinueLabels = []
  , symbols            = symbolTable
  , symbolsSize        = locNext
  }
  where (symbolTable, locNext) = buildSymbolTable symbols 0 0

buildSymbolTable :: [Grammar.Symbol] -> Int -> Int -> ([Symbol], Int)
buildSymbolTable symbols locLast locNext = case symbols of
  [] -> ([], locLast)
  (sym : syms) ->
    let
      Grammar.Symbol { Grammar.symName, Grammar.symDataType } = sym
      Grammar.DataType dims _ = symDataType
      size                    = product dims
      loc                     = locNext
      (syms', locLast)        = buildSymbolTable syms loc (loc + size)
    in
      ( (Symbol { symName, symDataType, symRelLoc = loc } : syms')
      , locLast
      )


--
codeStartAddr :: Int
codeStartAddr = 2056

xexeHeader :: [String]
xexeHeader = ["0", show codeStartAddr, "0", "0", "0", "0", "0", "0"]

getCodeTranslated :: Codegen [XSMInstr]
getCodeTranslated = do
  labels <- gets labels
  code   <- gets code
  let codeTranslated = labelTranslate codeStartAddr code labels
  return codeTranslated

getCodeLabelled :: Codegen [(String, XSMInstr)]
getCodeLabelled = do
  code   <- gets code
  labels <- gets (HM.toList . labels)
  return $ prependLabels code codeStartAddr labels

labelTranslate
  :: Int -> [XSMInstr] -> HM.HashMap String Int -> [XSMInstr]
labelTranslate offset instrs labels = map
  (\instr -> case instr of
    XSM_UTJ jmp ->
      let
        label = utjGetLabel jmp
        loc   = labels HM.! label
        loc'  = (loc * 2) + offset
      in utjTranslate jmp loc'
    _ -> instr
  )
  instrs

prependLabels
  :: [XSMInstr] -> Int -> [(String, Int)] -> [(String, XSMInstr)]
prependLabels code i labels =
  let labelsSorted = sortOn snd labels
  in
    case labelsSorted of
      []                     -> map ("", ) code
      ((label, j) : labels') -> case code of
        [] -> (label, XSM_NOP) : prependLabels [] (i + 1) labels'
        (c : code') -> if i == j
          then
            let c' = (label, c)
            in c' : prependLabels code' (i + 1) labels'
          else ("", c) : prependLabels code' (i + 1) labels

--

execSetupGlobalSymtab :: Codegen ()
execSetupGlobalSymtab = do
  symbolsSize <- gets symbolsSize
  appendCode [XSM_MOV_Int SP 4096, XSM_ADD_I SP symbolsSize]

execProgram :: Program -> Codegen ()
execProgram program = do
  let (Program stmts) = program
  mapM_ execStmt stmts

execStmt :: Stmt -> Codegen ()
execStmt stmt = case stmt of
  StmtAssign   stmt -> execStmtAssign stmt
  StmtRead     stmt -> execStmtRead stmt
  StmtWrite    stmt -> execStmtWrite stmt
  StmtIf       stmt -> execStmtIf stmt
  StmtIfElse   stmt -> execStmtIfElse stmt
  StmtWhile    stmt -> execStmtWhile stmt
  StmtBreak    stmt -> execStmtBreak stmt
  StmtContinue stmt -> execStmtContinue stmt

execStmtAssign :: StmtAssign -> Codegen ()
execStmtAssign stmt = do
  let (MkStmtAssign lhs rhs) = stmt
  rhsReg    <- getRValueInReg rhs
  lhsLocReg <- getLValueLocInReg lhs
  appendCode [XSM_MOV_IndDst lhsLocReg rhsReg]
  releaseReg lhsLocReg
  releaseReg rhsReg
  return ()

execStmtRead :: StmtRead -> Codegen ()
execStmtRead stmt = do
  let MkStmtRead lValue = stmt
  lValueLocReg <- getLValueLocInReg lValue
  t1           <- getFreeReg
  let
    code =
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

execStmtWrite :: StmtWrite -> Codegen ()
execStmtWrite stmt = do
  let MkStmtWrite rValue = stmt
  reg <- getRValueInReg rValue
  printReg reg
  releaseReg reg

execStmtIf :: StmtIf -> Codegen ()
execStmtIf stmt = do
  let MkStmtIf condition stmts = stmt
  condReg  <- getRValueInReg condition
  endLabel <- getNewLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JZ condReg endLabel]
  mapM_ execStmt stmts
  installLabel endLabel
  releaseReg condReg

execStmtIfElse :: StmtIfElse -> Codegen ()
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

loopBody :: (String -> String -> Codegen ()) -> Codegen ()
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

execStmtWhile :: StmtWhile -> Codegen ()
execStmtWhile stmt = do
  let MkStmtWhile condition stmts = stmt
  loopBody $ \startLabel endLabel -> do
    r <- getRValueInReg condition
    appendCode [XSM_UTJ $ XSM_UTJ_JZ r endLabel]
    releaseReg r
    mapM_ execStmt stmts
    appendCode [XSM_UTJ $ XSM_UTJ_JMP startLabel]

execStmtBreak :: StmtBreak -> Codegen ()
execStmtBreak _ = do
  endLabel <- peekLoopBreakLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]

execStmtContinue :: StmtContinue -> Codegen ()
execStmtContinue _ = do
  endLabel <- peekLoopContinueLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]

printReg :: Reg -> Codegen ()
printReg reg = do
  t1 <- getFreeReg
  let
    code =
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

getLValueLocInReg :: LValue -> Codegen Reg
getLValueLocInReg lValue = do
  let (LValue indices ident) = lValue
  (DataType dims _) <- getSymbolDataType ident
  (reg, _)          <- getLValueLocInReg' dims indices ident
  return reg
 where
  getLValueLocInReg'
    :: [Int] -> [RValue] -> String -> Codegen (Reg, Int)
  getLValueLocInReg' dims indices identName = case (dims, indices) of
    ([], []) -> do
      reg <- getFreeReg
      loc <- getSymbolLocInStack identName
      appendCode [XSM_MOV_Int reg loc]
      return (reg, 1)
    ([]    , _ : _) -> error $ "Codegen bug: Too many indices "
    (d : ds, []   ) -> do
      (reg, innerSize) <- getLValueLocInReg' ds indices identName
      return (reg, innerSize * d)
    (d : ds, i : is) -> do
      (reg, innerSize) <- getLValueLocInReg' ds is identName
      rhs              <- getRValueInReg i
      appendCode [XSM_MUL_I rhs innerSize]
      appendCode [XSM_ADD reg rhs]
      releaseReg rhs
      return (reg, innerSize * d)

getRValueInReg :: RValue -> Codegen Reg
getRValueInReg rValue = case rValue of
  RExp (ExpNum i) -> do
    reg <- getFreeReg
    appendCode [XSM_MOV_Int reg i]
    return reg
  RExp (ExpStr s) -> do
    reg <- getFreeReg
    appendCode [XSM_MOV_Str reg s]
    return reg
  RExp (MkExpArithmetic e1 op e2) ->
    execALUInstr (arithOpInstr op) e1 e2
  RExp (MkExpLogical e1 op e2) ->
    execALUInstr (logicOpInstr op) e1 e2
  RLValue lValue -> do
    reg <- getLValueLocInReg lValue
    appendCode [XSM_MOV_IndSrc reg reg]
    return reg

type ALUInstr = (Reg -> Reg -> XSMInstr)

execALUInstr :: ALUInstr -> RValue -> RValue -> Codegen Reg
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



--

getSymbol :: String -> Codegen Symbol
getSymbol name = do
  symbols <- gets symbols
  return $ fromJust $ find (\sym -> symName sym == name) symbols

getSymbolDataType :: String -> Codegen DataType
getSymbolDataType name = symDataType <$> getSymbol name

getSymbolLocInStack identName = (4096 +) <$> gets
  ( symRelLoc
  . fromJust
  . find (\s -> symName s == identName)
  . symbols
  )

getFreeReg = do
  compiler <- get
  case freeRegs compiler of
    (r : rs) -> do
      put $ compiler { freeRegs = rs }
      return r
    [] ->
      throwError $ Error.compilerError "out of registers" (Span 0 0)

releaseReg reg = do
  compiler <- get
  put compiler { freeRegs = pushFreeReg (freeRegs compiler) reg }
  where pushFreeReg regs r = r : regs

appendCode getInstrs' = do
  compiler <- get
  put compiler { code = code compiler ++ getInstrs' }

getNewLabel = do
  compiler <- get
  let newLabelNo = lastLabelNo compiler + 1
  put compiler { lastLabelNo = newLabelNo }
  return $ "L" ++ show newLabelNo

installLabel label = do
  compiler <- get
  let nextLineNo = length (code compiler)
  put compiler
    { labels = HM.insert label nextLineNo (labels compiler)
    }

pushLoopBreakLabel label = do
  compiler <- get
  put compiler
    { loopBreakLabels = loopBreakLabels compiler ++ [label]
    }

peekLoopBreakLabel = gets (last . loopBreakLabels)

popLoopBreakLabel = do
  compiler <- get
  put compiler { loopBreakLabels = init (loopBreakLabels compiler) }

pushLoopContinueLabel label = do
  compiler <- get
  put compiler
    { loopContinueLabels = loopContinueLabels compiler ++ [label]
    }

peekLoopContinueLabel = gets (last . loopContinueLabels)

popLoopContinueLabel = do
  compiler <- get
  put compiler
    { loopContinueLabels = init (loopContinueLabels compiler)
    }
