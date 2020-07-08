{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE TupleSections #-}


module Backend.Codegen where

import Backend.Instructions
import Backend.Reg
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List (find, sortOn)
import qualified Data.HashMap.Strict as HM
import qualified Grammar as G
import Safe
import Data.Function


import Debug.Trace
dbgs :: Show a => String -> a -> a
dbgs s v = trace (s ++ ": " ++ show v) v

type Codegen = State CodegenState

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
    , vTableSize :: Int
    , funcs :: [Func]
    , lSymbols :: Maybe [Symbol]
    , userTypes :: [UserType]
    , userTypeVTables :: [(String, [VTableEntry])]
    }

data Symbol =
  Symbol
    { symName :: String
    , symType :: G.Type2
    , symRelLoc :: Int
    }
    deriving (Show, Eq)

data Func = Func {
    funcName :: String
  , funcRetType :: G.Type1
  , funcBody :: [G.Stmt]
  , funcSymbols :: [Symbol]
  , funcLocalVarsSize :: Int
  , funcLabel :: String
}

data UserType =
  UserType
    { utName :: String
    , utFields :: [Symbol]
    , utFuncs :: [Func]
    , utParentName :: Maybe String
    , utSize :: Int
    }

data VTableEntry =
  VTableEntry
    { vteFuncName :: String
    , vteFuncLabel :: String
    }

--

compileXEXE :: G.Program -> [XSMInstr]
compileXEXE program =
  compileXEXE_ program (getCodeTranslated codeStartAddrXEXE)

compileXEXEUntranslated :: G.Program -> [(String, XSMInstr)]
compileXEXEUntranslated program = compileXEXE_ program getCodeLabelled

compileXEXE_ :: G.Program -> Codegen a -> a
compileXEXE_ program action =
  let (G.Program (G.SymTabGlobal gSymbols) funcs userTypes) = program
  in
    runCodegen
        (do
          execVTableSetup       --  \
          execSetupGlobalSymtab --   | >>>> IMPORTANT These has to be at the top 
          execCallMainFunc      --  /                 in this order
          execMethodDefs
          execFuncDefs
          action
        )
      $ initCodegenState gSymbols funcs userTypes


compileLibrary :: G.Program -> [XSMInstr]
compileLibrary program =
  let
    (G.Program (G.SymTabGlobal gSymbols) funcs userTypes) =
      checkLibraryProgram program
  in
    runCodegen
        (do
          execFuncDefs
          getCodeTranslated codeStartAddrLibrary
        )
      $ initCodegenState gSymbols funcs userTypes
 where
  checkLibraryProgram program@(G.Program (G.SymTabGlobal gSymbols) funcs _userTypes)
    | not (null gSymbols)
    = error "Global variables not allowed in library"
    | length funcs > 1
    = error "No extra functions allowed in library"
    | G.fDefName (head funcs) /= "library"
    = error "Library main function should be named 'library'"
    | otherwise
    = program


--

runCodegen :: Codegen a -> CodegenState -> a
runCodegen compiler state = evalState compiler state

initCodegenState
  :: [G.Symbol] -> [G.FuncDef] -> [G.UserType] -> CodegenState
initCodegenState symbols funcs userTypes = codegenStateDefault
  { gSymbols
  , gSymbolsSize
  , funcs           = funcs'
  , userTypes       = userTypes'
  , userTypeVTables
  , vTableSize
  }
 where
  (gSymbols, gSymbolsSize) = buildSymbolTable symbols 0
  funcs'                   = buildFuncsTable funcs ""
  userTypes'               = map convertUserType userTypes
  userTypeVTables =
    map (\ut -> (utName ut, buildVTable ut userTypes')) userTypes'
  vTableSize = sum (map length userTypeVTables)


codegenStateDefault :: CodegenState
codegenStateDefault = CodegenState
  { freeRegs           = [[R0 .. R19]]
  , usedRegs           = [[]]
  , code               = []
  , labels             = HM.empty
  , lastLabelNo        = 0
  , loopBreakLabels    = []
  , loopContinueLabels = []
  , gSymbols           = []
  , gSymbolsSize       = 0
  , lSymbols           = Nothing
  , funcs              = []
  , userTypes          = []
  , userTypeVTables    = []
  , vTableSize         = 0
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
      G.Symbol { G.symName, G.symType } = cur
      loc                               = snd prev
    in
      ( Symbol { symName, symType, symRelLoc = loc }
      , loc + dataTypeSize symType
      )
  dataTypeSize (G.Type2 dims _) = product dims

buildFuncsTable :: [G.FuncDef] -> String -> [Func]
buildFuncsTable funcs utName = case funcs of
  []           -> []
  (f : funcs') -> toFunc f : buildFuncsTable funcs' utName
 where
  toFunc f = case f of
    G.FuncDef { G.fDefName, G.fDefRetType, G.fDefBody, G.fDefArgsLen, G.fDefSyms }
      -> let
           (G.SymTabLocal fDefSyms') = fDefSyms
           (args, localVars)         = splitAt fDefArgsLen fDefSyms'
           args'                     = buildFuncArgsTable args (-4)
           (localVars', locNext)     = buildSymbolTable localVars 1
         in Func
           { funcName          = fDefName
           , funcRetType       = fDefRetType
           , funcBody          = fDefBody
           , funcSymbols       = args' ++ localVars'
           , funcLocalVarsSize = locNext - 1
           , funcLabel         = getMethodLabel utName fDefName
           }

buildFuncArgsTable :: [G.Symbol] -> Int -> [Symbol]
buildFuncArgsTable symbols locBase =
  let
    sentinel = Symbol
      { symName   = error "sentinel"
      , symType   = error "sentinel"
      , symRelLoc = locBase + 1
      }
  in init $ scanr f sentinel symbols
 where
  f cur prev =
    let
      G.Symbol { G.symName, G.symType } = cur
      G.Type2 dims _                    = symType
      size                              = product dims
      -- curLoc + curSize = prevLoc
      curLoc                            = symRelLoc prev - size
    in Symbol { symName, symType, symRelLoc = curLoc }



getMethodLabel :: String -> String -> String
getMethodLabel utName funcName = utName ++ "::" ++ funcName

convertUserType :: G.UserType -> UserType
convertUserType G.UserType { G.utName, G.utFields, G.utFuncs, G.utParentName }
  = let (utFields', utSize) = buildSymbolTable utFields 1
    in
      UserType
        { utName
        , utFields     = utFields'
        , utFuncs      = buildFuncsTable (map toFuncDef utFuncs) utName
        , utParentName
        , utSize
        }
 where
  toFuncDef (G.FuncDefined funcDef) = funcDef
  toFuncDef (_) = error "Function declared, but not defined"

buildVTable :: UserType -> [UserType] -> [VTableEntry]
buildVTable userType userTypes =
  let
    baseTable =
      case ((flip utLookup userTypes) <$> utParentName userType) of
        Just parentType -> (buildVTable parentType userTypes)
        Nothing         -> []
  in toVTable baseTable (utFuncs userType)
 where
  utLookup name userTypes = findJustNote
    ("Codegen: Find UserType by name: " ++ name)
    (\t -> utName t == name)
    userTypes
  toVTable baseTable [] = baseTable
  toVTable baseTable (func : fs) =
    let
      vte = VTableEntry
        { vteFuncName  = funcName func
        , vteFuncLabel = funcLabel func
        }
      baseTable' = insertList vteFuncName vte baseTable
    in toVTable baseTable' fs



--

codeStartAddrXEXE :: Int
codeStartAddrXEXE = 2056

codeStartAddrLibrary :: Int
codeStartAddrLibrary = 0

headerXEXE :: [String]
headerXEXE = ["0", show codeStartAddrXEXE, "0", "0", "0", "0", "0", "0"]

--

getCodeTranslated :: Int -> Codegen [XSMInstr]
getCodeTranslated startAddr = do
  labels <- gets labels
  code   <- gets code
  let codeTranslated = labelTranslate startAddr code labels
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
  vTableSize   <- gets vTableSize
  gSymbolsSize <- gets gSymbolsSize
  appendCode [XSM_MOV_Int SP (4096 + vTableSize), XSM_ADD_I SP gSymbolsSize]


execCallMainFunc :: Codegen ()
execCallMainFunc = do
  mainFunc <- findMain
  appendCode [XSM_UTJ $ XSM_UTJ_CALL (funcLabel mainFunc)]
  appendCode [XSM_INT 10]
 where
  findMain =
    gets
      $ findJustNote "main function not found" (\f -> funcName f == "main")
      . funcs

execFuncDefs :: Codegen ()
execFuncDefs = do
  funcs <- gets funcs
  mapM_ execFuncDef funcs

execMethodDefs :: Codegen ()
execMethodDefs = do
  userTypes <- gets userTypes
  mapM_ ((mapM_ execFuncDef) . utFuncs) userTypes

execVTableSetup :: Codegen ()
execVTableSetup = do
  vTables <- gets userTypeVTables
  r       <- getFreeReg
  appendCode [XSM_MOV_Int r 4096]
  mapM_ (execVTableSetup1 r) vTables
  releaseReg r

execVTableSetup1 :: Reg -> (a, [VTableEntry]) -> Codegen ()
execVTableSetup1 reg (_, vtes) = flip mapM_ vtes $ \vte -> do
  let label = vteFuncLabel vte
  t <- getFreeReg
  appendCode [XSM_UTJ $ XSM_UTJ_MOV t label]
  appendCode [XSM_MOV_IndDst reg t]
  appendCode [XSM_ADD_I reg 1]
  releaseReg t

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
  G.StmtAssign     stmt -> execStmtAssign stmt
  G.StmtRead       stmt -> execStmtRead stmt
  G.StmtWrite      stmt -> execStmtWrite stmt
  G.StmtIf         stmt -> execStmtIf stmt
  G.StmtIfElse     stmt -> execStmtIfElse stmt
  G.StmtWhile      stmt -> execStmtWhile stmt
  G.StmtBreak      stmt -> execStmtBreak stmt
  G.StmtContinue   stmt -> execStmtContinue stmt
  G.StmtReturn     stmt -> execStmtReturn stmt
  G.StmtRValue     stmt -> execStmtRValue stmt
  G.StmtInitialize stmt -> execStmtInitialize stmt
  G.StmtAlloc      stmt -> execStmtAlloc stmt
  G.StmtNew        stmt -> execStmtNew stmt
  G.StmtFree       stmt -> execStmtFree stmt
  G.StmtPoke       stmt -> execStmtPoke stmt


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
execStmtRead stmt = withSaveRegs $ \_retReg -> do
  let G.MkStmtRead lValue = stmt
  lValueLocReg <- getLValueLocInReg lValue
  t1           <- getFreeReg
  appendCode
    [ XSM_MOV_Str t1 "Read" -- arg1: Function Name
    , XSM_PUSH t1
    , XSM_MOV_Int t1 (-1) -- arg2: File Pointer (Stdin = -1)
    , XSM_PUSH t1
    , XSM_PUSH lValueLocReg -- arg3: Buffer loc
    , XSM_PUSH R0 -- arg4: unused
    , XSM_PUSH R0 -- arg5: unused
    , XSM_PUSH R0 -- selfDummy
    , XSM_CALL 0 -- call library
    , XSM_POP t1 -- selfDummy
    , XSM_POP t1 -- arg5
    , XSM_POP t1 -- arg4
    , XSM_POP t1 -- arg3
    , XSM_POP t1 -- arg2
    , XSM_POP t1 -- arg1
    ]
  return ((), True)


execStmtWrite :: G.StmtWrite -> Codegen ()
execStmtWrite stmt = withSaveRegs $ \_retReg -> do
  let G.MkStmtWrite rValue = stmt
  reg <- getRValueInReg rValue
  t1  <- getFreeReg
  appendCode
    [ XSM_MOV_Str t1 "Write" -- arg1: Call Number (Write = 5)
    , XSM_PUSH t1
    , XSM_MOV_Int t1 (-2) -- arg2: File Pointer (Stdout = -2)
    , XSM_PUSH t1
    , XSM_PUSH reg -- arg3: data to be written
    , XSM_PUSH R0 -- arg4: unused
    , XSM_PUSH R0 -- arg5: unused
    , XSM_PUSH R0 -- selfDummy
    , XSM_CALL 0 -- call library
    , XSM_POP t1 -- selfDummy
    , XSM_POP t1 -- arg5
    , XSM_POP t1 -- arg4
    , XSM_POP t1 -- arg3
    , XSM_POP t1 -- arg2
    , XSM_POP t1 -- arg1
    ]
  return ((), True)

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

execStmtInitialize :: G.StmtInitialize -> Codegen ()
execStmtInitialize _stmt = withSaveRegs $ \_retReg -> do
  t1 <- getFreeReg
  appendCode
    [ XSM_MOV_Str t1 "Heapset" -- arg1: Function Name
    , XSM_PUSH t1 -- arg1
    , XSM_PUSH R0 -- arg2: unused
    , XSM_PUSH R0 -- arg3: unused
    , XSM_PUSH R0 -- arg4: unused
    , XSM_PUSH R0 -- selfDummy
    , XSM_PUSH R0 -- space for return value: unused
    , XSM_CALL 0 -- call library
    , XSM_POP t1 -- return value
    , XSM_POP t1 -- selfDummy
    , XSM_POP t1 -- arg4
    , XSM_POP t1 -- arg3
    , XSM_POP t1 -- arg2
    , XSM_POP t1 -- arg1
    ]
  return ((), True)

execStmtAlloc :: G.StmtAlloc -> Codegen ()
execStmtAlloc stmt = withSaveRegs $ \retReg -> do
  let G.MkStmtAlloc lValue = stmt
  (reg, type_) <- getLValueLocInReg1 lValue
  userType     <- lookupUserType (getUserTypeName type_)
  let size = utSize userType
  backupRegs [reg]
  t1 <- getFreeReg
  appendCode
    [ XSM_MOV_Str t1 "Alloc" -- arg1: Function Name
    , XSM_PUSH t1 -- arg1
    , XSM_MOV_Int t1 size
    , XSM_PUSH t1 -- arg2: size
    , XSM_PUSH R0 -- arg3: unused
    , XSM_PUSH R0 -- arg4: unused
    , XSM_PUSH R0 -- selfDummy
    , XSM_PUSH R0 -- space for return value
    , XSM_CALL 0 -- call library
    , XSM_POP retReg -- return value
    , XSM_POP t1 -- selfDummy
    , XSM_POP t1 -- arg4
    , XSM_POP t1 -- arg3
    , XSM_POP t1 -- arg2
    , XSM_POP t1 -- arg1
    ]
  restoreRegs [reg]
  appendCode [XSM_MOV_IndDst reg retReg]
  return ((), True)

execStmtNew :: G.StmtNew -> Codegen ()
execStmtNew stmt = withSaveRegs $ \retReg -> do
  let G.MkStmtNew lValue targetType = stmt
  let
    targetTypeName = case targetType of
      G.TypeUser (inhChain) -> head inhChain
      _                     -> error "PQR"
  (reg, type_) <- getLValueLocInReg1 lValue
  userType     <- lookupUserType (getUserTypeName type_)
  let size = utSize userType
  backupRegs [reg]
  t1 <- getFreeReg
  appendCode
    [ XSM_MOV_Str t1 "Alloc" -- arg1: Function Name
    , XSM_PUSH t1 -- arg1
    , XSM_MOV_Int t1 size
    , XSM_PUSH t1 -- arg2: size
    , XSM_PUSH R0 -- arg3: unused
    , XSM_PUSH R0 -- arg4: unused
    , XSM_PUSH R0 -- selfDummy
    , XSM_PUSH R0 -- space for return value
    , XSM_CALL 0 -- call library
    , XSM_POP retReg -- return value
    , XSM_POP t1 -- selfDummy
    , XSM_POP t1 -- arg4
    , XSM_POP t1 -- arg3
    , XSM_POP t1 -- arg2
    , XSM_POP t1 -- arg1
    ]
  restoreRegs [reg]
  appendCode [XSM_MOV_IndDst reg retReg]

  -- Load VTable
  loc <- dbgs "VTableLoc" <$> findVTableLoc 4096 targetTypeName <$> gets userTypeVTables
  appendCode [XSM_MOV_Int reg loc]
  appendCode [XSM_MOV_IndDst retReg reg]

  return ((), True)
 where
  findVTableLoc _ utName [] =
    error $ "Can't find vtable entry for: " ++ utName
  findVTableLoc base utName ((utName', vtes) : vtess) =
    if utName == utName'
      then base
      else findVTableLoc (base + (length vtes)) utName vtess

execStmtFree :: G.StmtFree -> Codegen ()
execStmtFree stmt = withSaveRegs $ \_retReg -> do
  let G.MkStmtFree lValue = stmt
  reg <- getLValueLocInReg lValue
  t1  <- getFreeReg
  appendCode
    [ XSM_MOV_Str t1 "Free" -- arg1: Function Name
    , XSM_PUSH t1 -- arg1
    , XSM_MOV_IndSrc t1 reg
    , XSM_PUSH t1 -- arg2: address
    , XSM_PUSH R0 -- arg3: unused
    , XSM_PUSH R0 -- arg4: unused
    , XSM_PUSH R0 -- selfDummy
    , XSM_PUSH R0 -- space for return value
    , XSM_CALL 0 -- call library
    , XSM_POP t1 -- return value
    , XSM_POP t1 -- selfDummy
    , XSM_POP t1 -- arg4
    , XSM_POP t1 -- arg3
    , XSM_POP t1 -- arg2
    , XSM_POP t1 -- arg1
    ]
  return ((), True)

execStmtPoke :: G.StmtPoke -> Codegen ()
execStmtPoke stmt = do
  let G.MkStmtPoke dst src = stmt
  dstReg <- getRValueInReg dst
  srcReg <- getRValueInReg src
  appendCode [XSM_MOV_IndDst dstReg srcReg]


--

getLValueLocInReg :: G.LValue -> Codegen Reg
getLValueLocInReg a = fst <$> getLValueLocInReg1 a

getLValueLocInReg1 :: G.LValue -> Codegen (Reg, G.Type2)
getLValueLocInReg1 (G.LValueGlobal ident indices _ _) = do
  symbol <- fromJustNote "getLValueLocInReg1 GSymbol"
    <$> getGlobalSymbol ident
  let type_@(G.Type2 dims _) = symType symbol
  baseReg <- getFreeReg
  start   <- gets vTableSize
  appendCode [XSM_MOV_Int baseReg (4096 + start + symRelLoc symbol)]
  (reg, _) <- applyIndices baseReg dims indices ident
  return (reg, type_)
getLValueLocInReg1 (G.LValueLocal ident indices _ _) = do
  symbol <- fromJustNote "getLValueLocInReg1 LSymbol"
    <$> getLocalSymbol ident
  let type_@(G.Type2 dims _) = symType symbol
  baseReg <- getFreeReg
  appendCode [XSM_MOV_R baseReg BP]
  appendCode [XSM_ADD_I baseReg (symRelLoc symbol)]
  case symType symbol of
    (G.Type2 _ (G.TypeUser _)) -> if (symRelLoc symbol) < 0
      then appendCode [XSM_MOV_IndSrc baseReg baseReg]
      else return ()
    _ -> return ()
  (reg, _) <- applyIndices baseReg dims indices ident
  return (reg, type_)
getLValueLocInReg1 (G.LValueField ident indices _ _ lValue parentUTName) =
  do
    (baseReg, _) <- getLValueLocInReg1 lValue
    sym          <- getFieldSymbol parentUTName ident
    let type_@(G.Type2 dims _) = symType sym
    appendCode [XSM_MOV_IndSrc baseReg baseReg]
    appendCode [XSM_ADD_I baseReg (symRelLoc sym)]
    (reg, _) <- applyIndices baseReg dims indices ident
    return (reg, type_)
getLValueLocInReg1 (G.LValueSelf type_) = do
  symbol <- fromJustNote "getLValueLocInReg1 LSymbol" <$> getLocalSymbol ""
  baseReg <- getFreeReg
  appendCode [XSM_MOV_R baseReg BP]
  appendCode [XSM_ADD_I baseReg (symRelLoc symbol)]
  return (baseReg, type_)


applyIndices :: Reg -> [Int] -> [G.RValue] -> String -> Codegen (Reg, Int)
applyIndices baseReg dims indices symName = case (dims, indices) of
  ([]    , []   ) -> return (baseReg, 1)
  ([]    , _ : _) -> error "Codegen bug: Too many indices "
  (d : ds, []   ) -> do
    (reg, innerSize) <- applyIndices baseReg ds indices symName
    return (reg, innerSize * d)
  (d : ds, i : is) -> do
    (reg, innerSize) <- applyIndices baseReg ds is symName
    rhs              <- getRValueInReg i
    appendCode [XSM_MUL_I rhs innerSize]
    appendCode [XSM_ADD reg rhs]
    releaseReg rhs
    return (reg, innerSize * d)


lookupUserType :: String -> Codegen UserType
lookupUserType name = do
  gets
    ( findJustNote
        ("Codegen: Find UserType by name: " ++ name)
        (\t -> utName t == name)
    . userTypes
    )

getUserTypeName :: G.Type2 -> String
getUserTypeName type_ = case type_ of
  G.Type2 [] (G.TypeUser inhChain) -> head inhChain
  _ -> error $ "getUserTypeName: Not TypeUser: " ++ (show type_)


getLocalSymbol :: String -> Codegen (Maybe Symbol)
getLocalSymbol name =
  gets (\s -> find (\s -> (symName s) == name) =<< (lSymbols s))

getGlobalSymbol :: String -> Codegen (Maybe Symbol)
getGlobalSymbol name = gets (find (\s -> (symName s) == name) . gSymbols)

getFieldSymbol :: String -> String -> Codegen Symbol
getFieldSymbol utName fieldName = do
  userType <- lookupUserType utName
  return $ findJustNote
    "Codegen: Find member field"
    (\s -> symName s == fieldName)
    (utFields userType)




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


getUserType :: String -> Codegen UserType
getUserType name = gets
  ( findJustNote "Codegen: Find user type" (\s -> (utName s) == name)
  . userTypes
  )

--

backupRegs :: [Reg] -> Codegen ()
backupRegs regs = mapM_ (\reg -> appendCode [XSM_PUSH reg]) regs

restoreRegs :: [Reg] -> Codegen ()
restoreRegs regs = mapM_ (\reg -> appendCode [XSM_POP reg]) regs

withSaveRegs :: (Reg -> Codegen (a, Bool)) -> Codegen a
withSaveRegs action = do
  usedRegs_ <- getUsedRegs
  reg       <- getFreeReg
  backupRegs (filter ((/=) reg) usedRegs_)
  pushRegStack
  modify
    (\s -> s
      { freeRegs =
        (filter ((/=) reg) (head $ freeRegs s)) : (tail $ freeRegs s)
      , usedRegs = (reg : (head $ usedRegs s)) : (tail $ usedRegs s)
      }
    )
  (a, shouldReleaseReg) <- action reg
  popRegStack
  restoreRegs (filter ((/=) reg) usedRegs_)
  when shouldReleaseReg $ releaseReg reg
  return a


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
  G.RExp (G.MkExpArithmetic e1 op e2) ->
    execALUInstr (arithOpInstr op) e1 e2
  G.RExp (G.MkExpRelational e1 op e2) ->
    execALUInstr (relationalOpInstr op) e1 e2
  G.RExp (G.MkExpLogical e1 op e2) ->
    execALUInstr (logicalOpInstr op) e1 e2
  G.RLValue lValue -> do
    reg <- getLValueLocInReg lValue
    appendCode [XSM_MOV_IndSrc reg reg]
    return reg
  G.RFuncCall fname args _ -> withSaveRegs $ \retReg -> do
    label <- getFuncLabel fname
    mapM_
      (\arg -> do
        r1 <- case arg of
          (G.RLValue lValue) -> do
            (reg, typ) <- getLValueLocInReg1 lValue
            case typ of
              G.Type2 _ (G.TypeUser _) -> return reg
              _                        -> getRValueInReg arg
          _ -> getRValueInReg arg
        appendCode [XSM_PUSH r1]
        releaseReg r1
      )
      args
    appendCode [XSM_PUSH R0] -- selfDummy
    appendCode [XSM_PUSH R0] -- Space for return value
    appendCode [XSM_UTJ $ XSM_UTJ_CALL label]
    appendCode [XSM_POP retReg]
    t <- getFreeReg
    appendCode [XSM_POP t] -- selfDummy
    mapM_ (\_ -> appendCode [XSM_POP t]) args
    releaseReg t
    return (retReg, False)
  G.RMethodCall self fname args _ -> withSaveRegs $ \retReg -> do
    let
      selfUtName =
        G.lvType self
          & (\case
              (G.Type2 [] (G.TypeUser inhChain)) -> head inhChain
              _ -> error "UnexpectedRMC1"
            )
    selfLoc    <- getRValueInReg (G.RLValue self)
    vTableBase <- getFreeReg
    appendCode [XSM_MOV_IndSrc vTableBase selfLoc]
    vTable <-
      gets
      $ snd
      . (findJust (\(name, _) -> name == selfUtName))
      . userTypeVTables
    let offset = dbgs "Method offset " (findIndexJust (\vte -> vteFuncName vte == fname) vTable)
    appendCode [XSM_ADD_I vTableBase offset]
    mapM_
      (\arg -> do
        r1 <- case arg of
          (G.RLValue lValue) -> do
            (reg, typ) <- getLValueLocInReg1 lValue
            case typ of
              G.Type2 _ (G.TypeUser _) -> return reg
              _                        -> getRValueInReg arg
          _ -> getRValueInReg arg
        appendCode [XSM_PUSH r1]
        releaseReg r1
      )
      args
    appendCode [XSM_PUSH selfLoc] -- Self
    appendCode [XSM_PUSH R0] -- Space for return value
    appendCode [XSM_MOV_IndSrc vTableBase vTableBase]
    appendCode [XSM_CALLInd vTableBase]
    appendCode [XSM_POP retReg]
    t <- getFreeReg
    appendCode [XSM_POP t] -- Self
    mapM_ (\_ -> appendCode [XSM_POP t]) args
    releaseReg t
    releaseReg vTableBase
    releaseReg selfLoc
    return (retReg, False)
  G.RSyscall intNum callNum arg1 arg2 arg3 -> withSaveRegs $ \retReg -> do
    r1 <- getRValueInReg arg1
    r2 <- getRValueInReg arg2
    r3 <- getRValueInReg arg3
    t1 <- getFreeReg
    appendCode
      [ XSM_MOV_Int t1 callNum -- Call Number
      , XSM_PUSH t1
      , XSM_PUSH r1
      , XSM_PUSH r2 -- arg2: data to be written
      , XSM_PUSH r3 -- arg3: unused
      , XSM_PUSH t1 -- space for return value
      , XSM_INT intNum
      , XSM_POP retReg -- 
      , XSM_POP t1 -- arg3
      , XSM_POP t1 -- arg2
      , XSM_POP t1 -- arg1
      , XSM_POP t1 -- callNum
      ]
    return (retReg, False)
  G.RPeek rValue -> do
    r1 <- getRValueInReg rValue
    appendCode [XSM_MOV_IndSrc r1 r1]
    return r1
  G.RNull -> do
    r1 <- getFreeReg
    appendCode [XSM_MOV_Int r1 0]
    return r1

getFuncLabel :: String -> Codegen String
getFuncLabel name = gets
  ( funcLabel
  . findJustNote "Codegen: getFuncLabel" (\s -> funcName s == name)
  . funcs
  )

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
    _ -> error "out of registers"

releaseReg :: Reg -> Codegen ()
releaseReg reg = do
  compiler <- get
  let
    freeRegsHead : freeRegsTail = freeRegs compiler
    usedRegsHead : usedRegsTail = usedRegs compiler
    freeRegs'                   = (reg : freeRegsHead) : freeRegsTail
    usedRegs' = filter ((/=) reg) usedRegsHead : usedRegsTail
  put compiler { freeRegs = freeRegs', usedRegs = usedRegs' }

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

insertList :: Eq a => (t -> a) -> t -> [t] -> [t]
insertList key a list = case list of
  [] -> [a]
  (a' : as') ->
    if key a == key a' then a : as' else a' : insertList key a as'
