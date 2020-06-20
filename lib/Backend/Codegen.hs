{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RecordWildCards #-}

module Backend.Codegen where

import Backend.Instructions
import Backend.Reg
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List (find)
import Data.List (sortOn)
import Data.Maybe (fromJust)
import Error
import Span
import qualified Data.HashMap.Strict as HM
import qualified Grammar as G


-- import Debug.Trace
-- dbgs s v = trace (s ++ ": " ++ show v) v

type Codegen = StateT CodegenState (Either Error)

data CodegenState =
  CodegenState
    { freeRegs :: [[Reg]]
    , usedRegs :: [[Reg]]
    , code :: [XSMInstr]
    , labels :: HM.HashMap String Int
    , lastLabelNo :: Int
    , loopBreakLabels :: [String]
    , loopContinueLabels :: [String]
    , gSymbols :: [Symbol]
    , gSymbolsSize :: Int
    , funcs :: [Func]
    , lSymbols :: Maybe [Symbol]
    }

data Symbol =
  Symbol
    { symName :: String
    , symDataType :: G.DataType
    , symRelLoc :: Int
    }
    deriving (Show, Eq)

data Func = Func {
    funcName :: String
  , funcRetType :: G.PrimitiveType
  , funcBody :: [G.Stmt]
  , funcSymbols :: [Symbol]
  , funcLocalVarsSize :: Int
  , funcLabel :: String
}

runCodegen :: Codegen a -> CodegenState -> Either Error a
runCodegen compiler state = evalStateT compiler state


initCodegenState :: [G.Symbol] -> [G.FuncDef] -> CodegenState
initCodegenState symbols funcs = initCodegenStateInternal
  gSymbols
  gSymbolsSize
  funcs'
 where
  (gSymbols, gSymbolsSize) = buildSymbolTable symbols 0
  funcs'                   = buildFuncsTable funcs 0

initCodegenStateInternal :: [Symbol] -> Int -> [Func] -> CodegenState
initCodegenStateInternal gSymbols gSymbolsSize funcs = CodegenState
  { freeRegs           = [[R0 .. R19]]
  , usedRegs           = [[]]
  , code               = []
  , labels             = HM.empty
  , lastLabelNo        = 0
  , loopBreakLabels    = []
  , loopContinueLabels = []
  , gSymbols
  , gSymbolsSize
  , lSymbols           = Nothing
  , funcs
  }

buildSymbolTable :: [G.Symbol] -> Int -> ([Symbol], Int)
buildSymbolTable symbols locBase =
  let
    sentinel = (error "sentinel", locBase)
    syms'    = scanl f sentinel symbols
  in (map fst (tail syms'), snd $ last syms')
 where
  f prev cur =
    let
      G.Symbol { G.symName, G.symDataType } = cur
      loc = snd prev
    in
      ( Symbol { symName, symDataType, symRelLoc = loc }
      , loc + (dataTypeSize symDataType)
      )
  dataTypeSize (G.DataType dims _) = product dims

buildFuncsTable :: [G.FuncDef] -> Int -> [Func]
buildFuncsTable funcs i = case funcs of
  []           -> []
  (f : funcs') -> (toFunc f) : (buildFuncsTable funcs' (i + 1))
 where
  toFunc f = case f of
    G.FuncDef {fDefName, fDefRetType, fDefBody, fDefArgsLen, fDefSyms }
      -> let
           (args, localVars)     = splitAt fDefArgsLen fDefSyms
           args'                 = buildFuncArgsTable args (-3)
           (localVars', locNext) = buildSymbolTable localVars 1
         in Func
           { funcName = fDefName
           , funcRetType = fDefRetType
           , funcBody = fDefBody
           , funcSymbols       = args' ++ localVars'
           , funcLocalVarsSize = locNext - 1
           , funcLabel         = fDefName
           }

buildFuncArgsTable :: [G.Symbol] -> Int -> [Symbol]
buildFuncArgsTable symbols locBase =
  let
    sentinel = Symbol
      { symName     = error "sentinel"
      , symDataType = error "sentinel"
      , symRelLoc   = locBase + 1
      }
  in init $ scanr f sentinel symbols
 where
  f cur prev =
    let
      G.Symbol { G.symName, G.symDataType } = cur
      G.DataType dims _ = symDataType
      size              = product dims
      -- curLoc + curSize = prevLoc
      curLoc            = symRelLoc prev - size
    in Symbol { symName, symDataType, symRelLoc = curLoc }

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
  return $ prependLabels code 0 labels

labelTranslate :: Int -> [XSMInstr] -> HM.HashMap String Int -> [XSMInstr]
labelTranslate offset instrs labels = map
  (\instr -> case instr of
    XSM_UTJ jmp ->
      let
        label = utjGetLabel jmp
        loc   = labels HM.! label
        loc'  = (loc * 2) + offset
      in utjTranslate jmp loc'
      -- s = "label: " ++ label ++ "; loc: " ++ (show loc) ++ "; loc': " ++ (show loc')
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
        []          -> (label, XSM_NOP) : prependLabels [] (i + 1) labels'
        (c : code') -> if i == j
          then
            let c' = (label, c)
            in c' : prependLabels code' (i + 1) labels'
          else ("", c) : prependLabels code' (i + 1) labels

--

execSetupGlobalSymtab :: Codegen ()
execSetupGlobalSymtab = do
  gSymbolsSize <- gets gSymbolsSize
  appendCode [XSM_MOV_Int SP 4096, XSM_ADD_I SP gSymbolsSize]



execCallMainFunc :: Codegen ()
execCallMainFunc = do
  funcs <- gets funcs
  let
    mainFunc = case find (\f -> funcName f == "main") funcs of
      Just f  -> f
      Nothing -> error "main function not defined"
  appendCode [XSM_UTJ $ XSM_UTJ_CALL (funcLabel mainFunc)]
  appendCode [XSM_INT 10]

execFuncDefs :: Codegen ()
execFuncDefs = do
  funcs <- gets funcs
  mapM_ execFuncDef funcs

execFuncDef :: Func -> Codegen ()
execFuncDef func = do
  pushRegStack
  installLabel (funcLabel func)
  modify (\s -> s { lSymbols = Just $ funcSymbols func })
  appendCode [XSM_PUSH BP]
  appendCode [XSM_MOV_R BP SP]
  appendCode [XSM_ADD_I SP (funcLocalVarsSize func)]
  mapM_ execStmt (funcBody func)
  appendCode [XSM_MOV_R SP BP]
  appendCode [XSM_POP BP]
  appendCode [XSM_RET]
  modify (\s -> s { lSymbols = Nothing })
  popRegStack

execStmt :: G.Stmt -> Codegen ()
execStmt stmt = case stmt of
  G.StmtAssign   stmt -> execStmtAssign stmt
  G.StmtRead     stmt -> execStmtRead stmt
  G.StmtWrite    stmt -> execStmtWrite stmt
  G.StmtIf       stmt -> execStmtIf stmt
  G.StmtIfElse   stmt -> execStmtIfElse stmt
  G.StmtWhile    stmt -> execStmtWhile stmt
  G.StmtBreak    stmt -> execStmtBreak stmt
  G.StmtContinue stmt -> execStmtContinue stmt
  G.StmtReturn   stmt -> execStmtReturn stmt
  G.StmtRValue   stmt -> execStmtRValue stmt

execStmtAssign :: G.StmtAssign -> Codegen ()
execStmtAssign stmt = do
  let (G.MkStmtAssign lhs rhs) = stmt
  rhsReg    <- getRValueInReg rhs
  lhsLocReg <- getLValueLocInReg lhs
  appendCode [XSM_MOV_IndDst lhsLocReg rhsReg]
  releaseReg lhsLocReg
  releaseReg rhsReg
  return ()

execStmtRead :: G.StmtRead -> Codegen ()
execStmtRead stmt = do
  usedRegs <- getUsedRegs
  backupRegs usedRegs
  pushRegStack
  let G.MkStmtRead lValue = stmt
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
  restoreRegs usedRegs
  popRegStack

execStmtWrite :: G.StmtWrite -> Codegen ()
execStmtWrite stmt = do
  usedRegs <- getUsedRegs
  backupRegs usedRegs
  pushRegStack
  let G.MkStmtWrite rValue = stmt
  reg <- getRValueInReg rValue
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
  releaseReg reg
  restoreRegs usedRegs
  popRegStack

execStmtIf :: G.StmtIf -> Codegen ()
execStmtIf stmt = do
  let G.MkStmtIf condition stmts = stmt
  condReg  <- getRValueInReg condition
  endLabel <- getNewLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JZ condReg endLabel]
  mapM_ execStmt stmts
  installLabel endLabel
  releaseReg condReg

execStmtIfElse :: G.StmtIfElse -> Codegen ()
execStmtIfElse stmt = do
  let G.MkStmtIfElse condition stmtsThen stmtsElse = stmt
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

execStmtWhile :: G.StmtWhile -> Codegen ()
execStmtWhile stmt = do
  let G.MkStmtWhile condition stmts = stmt
  loopBody $ \startLabel endLabel -> do
    r <- getRValueInReg condition
    appendCode [XSM_UTJ $ XSM_UTJ_JZ r endLabel]
    releaseReg r
    mapM_ execStmt stmts
    appendCode [XSM_UTJ $ XSM_UTJ_JMP startLabel]

execStmtBreak :: G.StmtBreak -> Codegen ()
execStmtBreak _ = do
  endLabel <- peekLoopBreakLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]

execStmtContinue :: G.StmtContinue -> Codegen ()
execStmtContinue _ = do
  endLabel <- peekLoopContinueLabel
  appendCode [XSM_UTJ $ XSM_UTJ_JMP endLabel]

execStmtReturn :: G.StmtReturn -> Codegen ()
execStmtReturn stmt = do
  let (G.MkStmtReturn rValue) = stmt
  r1 <- getRValueInReg rValue
  t  <- getFreeReg
  appendCode [XSM_MOV_R t BP]
  appendCode [XSM_SUB_I t 2]
  appendCode [XSM_MOV_IndDst t r1]
  releaseReg t
  releaseReg r1
  appendCode [XSM_MOV_R SP BP]
  appendCode [XSM_POP BP]
  appendCode [XSM_RET]

execStmtRValue :: G.StmtRValue -> Codegen ()
execStmtRValue stmt = do
  let (G.MkStmtRValue rValue) = stmt
  r1 <- getRValueInReg rValue
  releaseReg r1

getLValueLocInReg :: G.LValue -> Codegen Reg
getLValueLocInReg lValue = do
  let (G.LValue indices ident) = lValue
  (G.DataType dims _) <- getSymbolDataType ident
  (reg, _)          <- getLValueLocInReg' dims indices ident
  return reg
 where
  getLValueLocInReg' :: [Int] -> [G.RValue] -> String -> Codegen (Reg, Int)
  getLValueLocInReg' dims indices symName = case (dims, indices) of
    ([], []) -> do
      reg <- getSymbolLocInReg symName
      return (reg, 1)
    ([]    , _ : _) -> error "Codegen bug: Too many indices "
    (d : ds, []   ) -> do
      (reg, innerSize) <- getLValueLocInReg' ds indices symName
      return (reg, innerSize * d)
    (d : ds, i : is) -> do
      (reg, innerSize) <- getLValueLocInReg' ds is symName
      rhs              <- getRValueInReg i
      appendCode [XSM_MUL_I rhs innerSize]
      appendCode [XSM_ADD reg rhs]
      releaseReg rhs
      return (reg, innerSize * d)

backupRegs :: [Reg] -> Codegen ()
backupRegs regs =
    mapM_ (\reg -> appendCode [XSM_PUSH reg]) regs

restoreRegs :: [Reg] -> Codegen ()
restoreRegs regs =
    mapM_ (\reg -> appendCode [XSM_POP reg]) regs

getRValueInReg :: G.RValue -> Codegen Reg
getRValueInReg rValue = case rValue of
  G.RExp (G.ExpNum i) -> do
    reg <- getFreeReg
    appendCode [XSM_MOV_Int reg i]
    return reg
  G.RExp (G.ExpStr s) -> do
    reg <- getFreeReg
    appendCode [XSM_MOV_Str reg s]
    return reg
  G.RExp (G.MkExpArithmetic e1 op e2) -> execALUInstr (arithOpInstr op) e1 e2
  G.RExp (G.MkExpRelational e1 op e2) ->
    execALUInstr (relationalOpInstr op) e1 e2
  G.RExp    (G.MkExpLogical e1 op e2) -> execALUInstr (logicalOpInstr op) e1 e2
  G.RLValue lValue                  -> do
    reg <- getLValueLocInReg lValue
    appendCode [XSM_MOV_IndSrc reg reg]
    return reg
  G.RFuncCall fname args -> do
    usedRegs <- getUsedRegs
    backupRegs usedRegs
    label <- getFuncLabel fname
    mapM_
      (\arg -> do
        r1 <- getRValueInReg arg
        appendCode [XSM_PUSH r1]
        releaseReg r1
      )
      args
    appendCode [XSM_PUSH R0] -- Space for return value
    appendCode [XSM_UTJ $ XSM_UTJ_CALL label]
    r1 <- getFreeReg
    appendCode [XSM_POP r1]
    t <- getFreeReg
    mapM_
      (\_ -> do
        appendCode [XSM_POP t]
      )
      args
    releaseReg t
    return r1

getFuncLabel :: String -> Codegen String
getFuncLabel name =
  gets (funcLabel . fromJust . find (\s -> funcName s == name) . funcs)

type ALUInstr = (Reg -> Reg -> XSMInstr)

execALUInstr :: ALUInstr -> G.RValue -> G.RValue -> Codegen Reg
execALUInstr instr e1 e2 = do
  r1 <- getRValueInReg e1
  r2 <- getRValueInReg e2
  appendCode [instr r1 r2]
  releaseReg r2
  return r1

arithOpInstr :: G.OpArithmetic -> ALUInstr
arithOpInstr op = case op of
  G.OpAdd -> XSM_ADD
  G.OpSub -> XSM_SUB
  G.OpMul -> XSM_MUL
  G.OpDiv -> XSM_DIV
  G.OpMod -> XSM_MOD

relationalOpInstr :: G.OpRelational -> ALUInstr
relationalOpInstr op = case op of
  G.OpLT -> XSM_LT
  G.OpGT -> XSM_GT
  G.OpLE -> XSM_LE
  G.OpGE -> XSM_GE
  G.OpNE -> XSM_NE
  G.OpEQ -> XSM_EQ



logicalOpInstr :: G.OpLogical -> ALUInstr
logicalOpInstr op = case op of
  G.OpLAnd -> XSM_MUL
  G.OpLOr  -> XSM_ADD


--

getSymbol :: String -> Codegen Symbol
getSymbol name = do
  lSymbol <- gets
    (\s -> join $ find (\s -> (symName s) == name) <$> (lSymbols s))
  gSymbol <- gets (\s -> find (\s -> (symName s) == name) $ (gSymbols s))
  case (lSymbol, gSymbol) of
    (Just symbol, _          ) -> return symbol
    (Nothing    , Just symbol) -> return symbol
    (Nothing    , Nothing    ) -> error $ "Symbol not found:" ++ name

getSymbolDataType :: String -> Codegen G.DataType
getSymbolDataType name = symDataType <$> getSymbol name

{-

Arg 1
Arg 2
Arg 3
Return Value Space
RET IP SAVE
BP SAVE                 <- BP
   --
Local Var 1
Local Var 2

-}

getSymbolLocInReg :: String -> Codegen Reg
getSymbolLocInReg name = do
  lSymbol <- gets (find (\s -> symName s == name) <=< lSymbols)
  gSymbol <- gets (find (\s -> symName s == name) . gSymbols)
  case (lSymbol, gSymbol) of
    (Just symbol, _) -> do
      r <- getFreeReg
      appendCode [XSM_MOV_R r BP]
      appendCode [XSM_ADD_I r (symRelLoc symbol)]
      return r
    (Nothing, Just symbol) -> do
      r <- getFreeReg
      appendCode [XSM_MOV_Int r (4096 + symRelLoc symbol)]
      return r
    (Nothing, Nothing) -> error $ "Symbol not found:" ++ name


getFreeReg :: Codegen Reg
getFreeReg = do
  compiler <- get
  case freeRegs compiler of
    (r : rs) : freeRegsTail -> do
      let
        freeRegs'                     = rs : freeRegsTail
        (usedRegsHead : usedRegsTail) = usedRegs compiler
        usedRegs'                     = (r : usedRegsHead) : usedRegsTail
      put $ compiler { freeRegs = freeRegs', usedRegs = usedRegs' }
      return r
    _ -> throwError $ Error.compilerError "out of registers" (Span 0 0)

releaseReg :: Reg -> Codegen ()
releaseReg reg = do
  compiler <- get
  let
    freeRegsHead : freeRegsTail = freeRegs compiler
    usedRegsHead : usedRegsTail = usedRegs compiler
    freeRegs'                   = (reg : freeRegsHead) : freeRegsTail
    usedRegs' = (filter ((/=) reg) usedRegsHead) : usedRegsTail
  put compiler
    { freeRegs = freeRegs'
    , usedRegs = usedRegs'
    }

pushRegStack :: Codegen ()
pushRegStack = modify $ \compiler -> compiler
  { freeRegs = [R0 .. R19] : freeRegs compiler
  , usedRegs = [] : usedRegs compiler
  }

popRegStack :: Codegen ()
popRegStack = modify $ \compiler -> compiler
  { freeRegs = tail $ freeRegs compiler
  , usedRegs = tail $ usedRegs compiler
  }

getUsedRegs :: Codegen [Reg]
getUsedRegs = gets (head . usedRegs)

appendCode :: [XSMInstr] -> Codegen ()
appendCode getInstrs' = do
  compiler <- get
  put compiler { code = code compiler ++ getInstrs' }

getNewLabel :: Codegen String
getNewLabel = do
  compiler <- get
  let newLabelNo = lastLabelNo compiler + 1
  put compiler { lastLabelNo = newLabelNo }
  return $ "L" ++ show newLabelNo

installLabel :: String -> Codegen ()
installLabel label = do
  compiler <- get
  let nextLineNo = length (code compiler)
  put compiler { labels = HM.insert label nextLineNo (labels compiler) }

pushLoopBreakLabel :: String -> Codegen ()
pushLoopBreakLabel label = do
  compiler <- get
  put compiler { loopBreakLabels = loopBreakLabels compiler ++ [label] }

peekLoopBreakLabel :: Codegen String
peekLoopBreakLabel = gets (last . loopBreakLabels)

popLoopBreakLabel :: Codegen ()
popLoopBreakLabel = do
  compiler <- get
  put compiler { loopBreakLabels = init (loopBreakLabels compiler) }

pushLoopContinueLabel :: String -> Codegen ()
pushLoopContinueLabel label = do
  compiler <- get
  put compiler
    { loopContinueLabels = loopContinueLabels compiler ++ [label]
    }

peekLoopContinueLabel :: Codegen String
peekLoopContinueLabel = gets (last . loopContinueLabels)

popLoopContinueLabel :: Codegen ()
popLoopContinueLabel = do
  compiler <- get
  put compiler { loopContinueLabels = init (loopContinueLabels compiler) }
