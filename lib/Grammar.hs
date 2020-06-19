{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PatternSynonyms          #-}

module Grammar where

import Control.Monad (unless)
import Control.Monad.Extra (unlessM)
import Data.Maybe (fromJust, isJust)
import Data.List (find)
import Error (Error)
import qualified Error
import Span
import Data.Bifunctor (second)

--


class Monad m => GrammarM m where
  gsGet :: m GrammarState
  gsPut :: GrammarState -> m ()
  gThrowError :: Error -> m a


gsGets :: GrammarM m => (GrammarState -> a) -> m a
gsGets f = f <$> gsGet

gsModify :: GrammarM m => (GrammarState -> GrammarState) -> m ()
gsModify f = (f <$> gsGet) >>= gsPut

data GrammarState =
  GrammarState
    { gsGSymbols :: [Symbol]
    , gsFuncs :: [Func]
    , gsFuncContext :: Maybe (FuncDecl, [Symbol])
    , gsLoopStack :: Int
    }

gsInit = GrammarState
  { gsGSymbols    = []
  , gsFuncs       = []
  , gsFuncContext = Nothing
  , gsLoopStack   = 0
  }

insertList key a list = case list of
  [] -> [a]
  (a' : as') ->
    (if key a == key a' then a : as' else a' : insertList key a as')

gSymLookup name = gsGets $ (find $ \s -> symName s == name) . gsGSymbols
gSymInsert symbol = gsModify
  $ \gs -> gs { gsGSymbols = insertList symName symbol (gsGSymbols gs) }

funcLookup name =
  gsGets $ (find $ \s -> (funcName . funcDecl $ s) == name) . gsFuncs

funcInsert func = gsModify $ \gs ->
  gs { gsFuncs = insertList (funcName . funcDecl) func (gsFuncs gs) }

hasLoop :: GrammarM m => m Bool
hasLoop = gsGets (\gs -> gsLoopStack gs >= 0)

pushLoop :: GrammarM m => m ()
pushLoop = gsModify (\gs -> gs { gsLoopStack = gsLoopStack gs + 1 })

popLoop :: GrammarM m => m ()
popLoop =
  gsModify (\gs -> gs { gsLoopStack = max 0 (gsLoopStack gs - 1) })

fcEnter :: GrammarM m => FuncDecl -> m ()
fcEnter funcDecl =
  gsModify $ \s -> s { gsFuncContext = Just (funcDecl, []) }

fcExit :: GrammarM m => m [Symbol]
fcExit = do
  (_, syms) <- gsGets (fromJust . gsFuncContext)
  gsModify $ \s -> s { gsFuncContext = Nothing }
  return syms

fcGet :: GrammarM m => m FuncDecl
fcGet = gsGets (fst . fromJust . gsFuncContext)

fcSymLookup :: GrammarM m => String -> m (Maybe Symbol)
fcSymLookup name =
  gsGets
    $ (find $ \s -> symName s == name)
    . snd
    . fromJust
    . gsFuncContext

fcSymInsert :: GrammarM m => Symbol -> m ()
fcSymInsert symbol = gsModify $ \gs -> gs
  { gsFuncContext = fmap
    (second $ insertList symName symbol)
    (gsFuncContext gs)
  }

fcHasCtxt :: GrammarM m => m Bool
fcHasCtxt = gsGets (isJust . gsFuncContext)

--

data Stmt
  = StmtAssign StmtAssign
  | StmtRead StmtRead
  | StmtWrite StmtWrite
  | StmtIf StmtIf
  | StmtIfElse StmtIfElse
  | StmtWhile StmtWhile
  | StmtBreak StmtBreak
  | StmtContinue StmtContinue
  | StmtRValue StmtRValue -- For function calls which is both an RValue and Statement
  | StmtReturn StmtReturn
  deriving (Show, Eq)

data StmtAssign
  = MkStmtAssign LValue RValue
  deriving (Show, Eq)

data StmtRead
  = MkStmtRead LValue
  deriving (Show, Eq)

data StmtWrite
  = MkStmtWrite RValue
  deriving (Show, Eq)

data StmtIf
  = MkStmtIf RValue [Stmt]
  deriving (Show, Eq)

data StmtIfElse
  = MkStmtIfElse RValue [Stmt] [Stmt]
  deriving (Show, Eq)

data StmtWhile
  = MkStmtWhile RValue [Stmt]
  deriving (Show, Eq)

data StmtBreak
  = MkStmtBreak
  deriving (Show, Eq)

data StmtContinue
  = MkStmtContinue
  deriving (Show, Eq)

data StmtReturn =
  MkStmtReturn RValue
  deriving (Show, Eq)

data StmtRValue =
  MkStmtRValue RValue
  deriving (Show, Eq)

data LValue
  = LValue
      [RValue] -- Indices, empty if simple ident
      String
  deriving (Show, Eq)

data RValue
  = RLValue LValue
  | RExp Exp
  | RFuncCall String [RValue]
  deriving (Show, Eq)

data Exp
  = ExpNum Int
  | ExpStr String
  | MkExpArithmetic RValue OpArithmetic RValue
  | MkExpRelational RValue OpRelational RValue
  | MkExpLogical RValue OpLogical RValue
  deriving (Show, Eq)

data OpArithmetic
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  deriving (Show, Eq)

data OpRelational
  = OpLT
  | OpGT
  | OpLE
  | OpGE
  | OpEQ
  | OpNE
  deriving (Show, Eq)

data OpLogical
  = OpLAnd
  | OpLOr
  deriving (Show, Eq)

data Symbol =
  Symbol
    { symName :: String
    , symDataType :: DataType
    , symDeclSpan :: Span
    }
  deriving (Show, Eq)

data Func
  = FuncDeclared FuncDecl
  | FuncDefined FuncDecl FuncDef

funcDecl :: Func -> FuncDecl
funcDecl (FuncDeclared x ) = x
funcDecl (FuncDefined x _) = x

data FuncDecl
  = FuncDecl { funcName     :: String
             , funcRetType  :: PrimitiveType
             , funcArgTypes :: [SpanW PrimitiveType]
             , funcDeclSpan :: Span
             }
  deriving (Show, Eq)

data FuncDef
  = FuncDef { funcBody      :: [Stmt]
            , funcArgsLen   :: Int
            , funcSyms :: [Symbol]
            , funcDefSpan   :: Span
            }

data PrimitiveType = TypeInt | TypeBool | TypeString | TypeUser String
  deriving (Eq)

data DataType
  = DataType
      [Int] -- Dims
      PrimitiveType
  deriving (Eq)

data UserType =
  UserType
    { utName :: String
    , utFields :: [Symbol]
    }

doVarDeclare
  :: GrammarM m => String -> PrimitiveType -> [Int] -> Span -> m ()
doVarDeclare identName primType dims span = do
  fcHasCtxt <- fcHasCtxt
  if fcHasCtxt
    then do
      fcSymLookup identName >>= throwSymbolExists
      fcSymInsert symbol
      return $ ()
    else do
      gSymLookup identName >>= throwSymbolExists
      gSymInsert symbol
      return $ ()
 where
  dataType = DataType dims primType
  symbol   = Symbol
    { symName     = identName
    , symDeclSpan = span
    , symDataType = dataType
    }
  throwSymbolExists maybeSym = case maybeSym of
    Nothing  -> return ()
    Just sym -> gThrowError
      $ errIdentifierRedeclared identName (symDeclSpan sym) span

mkStmtAssign :: GrammarM m => LValue -> RValue -> Span -> m StmtAssign
mkStmtAssign lhs rhs span = do
  lhsType     <- lValueDataType lhs
  lhsDeclSpan <- lValueDeclSpan lhs
  rhsType     <- rValueDataType rhs
  let (DataType dims _) = lhsType
  unless (length dims == 0)
    $ gThrowError --
    $ errArrayNotAllowed span lhsDeclSpan
  unless (lhsType == rhsType)
    $ gThrowError --
    $ errTypeMismatch lhsType lhsDeclSpan rhsType span
  return $ MkStmtAssign lhs rhs

mkStmtRead :: GrammarM m => SpanW LValue -> m StmtRead
mkStmtRead (SpanW lValue lValueSpan) = do
  dataType <- lValueDataType lValue
  let allowedTypes = [DataType [] TypeInt, DataType [] TypeString]
  unless (dataType `elem` allowedTypes)
    $ gThrowError --
    $ errTypeNotAllowed allowedTypes dataType lValueSpan
  return $ MkStmtRead lValue

mkStmtWrite :: GrammarM m => SpanW RValue -> m StmtWrite
mkStmtWrite (SpanW rValue rValueSpan) = do
  dataType <- rValueDataType rValue
  let allowedTypes = [DataType [] TypeInt, DataType [] TypeString]
  unless (dataType `elem` allowedTypes)
    $ gThrowError --
    $ errTypeNotAllowed allowedTypes dataType rValueSpan
  return $ MkStmtWrite rValue

mkStmtIf :: GrammarM m => SpanW RValue -> [Stmt] -> m StmtIf
mkStmtIf (SpanW cond span) body = do
  dataType <- rValueDataType cond
  unless (dataType == DataType [] TypeBool)
    $ gThrowError
    $ errTypeNotAllowed [DataType [] TypeBool] dataType span
  return $ MkStmtIf cond body

mkStmtIfElse
  :: GrammarM m => SpanW RValue -> [Stmt] -> [Stmt] -> m StmtIfElse
mkStmtIfElse (SpanW cond span) thenBody elseBody = do
  dataType <- rValueDataType cond
  unless (dataType == DataType [] TypeBool)
    $ gThrowError
    $ errTypeNotAllowed [DataType [] TypeBool] dataType span
  return $ MkStmtIfElse cond thenBody elseBody

mkStmtWhile :: GrammarM m => SpanW RValue -> [Stmt] -> m StmtWhile
mkStmtWhile (SpanW cond span) body = do
  dataType <- rValueDataType cond
  unless (dataType == DataType [] TypeBool)
    $ gThrowError
    $ errTypeNotAllowed [DataType [] TypeBool] dataType span
  return $ MkStmtWhile cond body

mkStmtBreak :: GrammarM m => Span -> m StmtBreak
mkStmtBreak span = do
  unlessM hasLoop throwOutOfLoop
  return $ MkStmtBreak
  where throwOutOfLoop = gThrowError (Error.syntaxError span)

mkStmtContinue :: GrammarM m => Span -> m StmtContinue
mkStmtContinue span = do
  unlessM hasLoop throwOutOfLoop
  return $ MkStmtContinue
  where throwOutOfLoop = gThrowError (Error.syntaxError span)

mkStmtRValue :: Monad m => RValue -> m StmtRValue
mkStmtRValue rValue = do
  return $ MkStmtRValue rValue

mkStmtReturn :: GrammarM m => RValue -> Span -> m StmtReturn
mkStmtReturn rValue span = do
  funcDecl <- fcGet
  let retType = DataType [] (funcRetType funcDecl)
  rValueType <- rValueDataType rValue
  unless (retType == rValueType)
    $ gThrowError --
    $ errTypeMismatch retType (funcDeclSpan funcDecl) rValueType span
  return $ MkStmtReturn rValue

doFuncDeclare
  :: GrammarM m
  => PrimitiveType
  -> String
  -> [SpanW PrimitiveType]
  -> Span
  -> m ()
doFuncDeclare retType funcName argTypes span = do
  funcLookup funcName >>= throwFuncRedeclared
  let
    func = FuncDecl
      { funcName     = funcName
      , funcRetType  = retType
      , funcArgTypes = argTypes
      , funcDeclSpan = span
      }
  funcInsert $ FuncDeclared func
  return $ ()
 where
  throwFuncRedeclared func = case func of
    Nothing -> return ()
    Just (FuncDeclared funcDec) ->
      gThrowError $ errFuncRedeclared span (funcDeclSpan funcDec)
    Just (FuncDefined funcDec _) ->
      gThrowError $ errFuncRedeclared span (funcDeclSpan (funcDec))

doFuncDefine
  :: GrammarM m
  => PrimitiveType
  -> String
  -> [SpanW (String, PrimitiveType)]
  -> Span
  -> m ([Stmt] -> m ())
doFuncDefine retType name args span = do
  funcDecl <-
    funcLookup name
    >>= throwRedefined
    >>= (throwMismatch retType name args)
    >>= declareIfNotDeclared
  fcEnter funcDecl
  flip mapM_ args $ \(SpanW (name, primType) span) -> fcSymInsert $ Symbol
    { symName     = name
    , symDataType = DataType [] primType
    , symDeclSpan = span
    }
  return $ \stmts -> do
    syms <- fcExit
    let
      funcDef = FuncDef
        { funcArgsLen = length args
        , funcSyms    = syms
        , funcBody    = stmts
        , funcDefSpan = span
        }
    funcInsert (FuncDefined funcDecl funcDef)
 where
  throwRedefined maybeFunc = case maybeFunc of
    Nothing -> return Nothing
    Just (FuncDefined _ func') ->
      gThrowError $ errFuncRedefined span (funcDefSpan func')
    Just (FuncDeclared func') -> do
      return $ Just func'
  throwMismatch retType name args maybeFuncDec = case maybeFuncDec of
    Nothing         -> return Nothing
    Just (funcDecl) -> do
      unless
          (and
            [ funcName funcDecl == name
            , funcRetType funcDecl == retType
            , (spanWVal <$> funcArgTypes funcDecl)
              == (snd . spanWVal <$> args)
            ]
          )
        $ gThrowError
        $ errFuncDefMismatch span (funcDeclSpan funcDecl)
      return $ Just funcDecl
  declareIfNotDeclared maybeFunc = case maybeFunc of
    Nothing -> do
      doFuncDeclare retType name (fmap (fmap snd) args) span
      func <- funcLookup name
      return $ funcDecl . fromJust $ func
    Just f -> return f

mkLValue :: GrammarM m => SpanW String -> [SpanW RValue] -> m LValue
mkLValue (SpanW identName identSpan) indices = do
  sym <- symLookupCombined identName >>= \case
    Nothing  -> gThrowError $ errIdentifierNotDeclared identName identSpan
    Just sym -> return sym
  let DataType dims _ = symDataType sym
  unless (length indices <= length dims)
    $ gThrowError (Error.customError "Too much indices" identSpan) -- TODO Better error
  indices' <- mapM
    (\(SpanW index indexSpan) -> do
      indexType <- rValueDataType index
      unless (indexType == DataType [] TypeInt) $ gThrowError
        (Error.customError "mkArrayIndex: index not int" indexSpan) -- TODO Better error
      return $ index
    )
    indices
  return $ LValue indices' identName

mkExpFuncCall :: GrammarM m => String -> [SpanW RValue] -> Span -> m RValue
mkExpFuncCall funcName args span = do
  func <- funcLookup funcName >>= throwFuncNotDeclared
  unless (length args == (length $ funcArgTypes . funcDecl $ func))
    $ gThrowError
    $ errFuncArgLengthMismatch
        (length $ funcArgTypes . funcDecl $ func)
        (length args)
        (funcDeclSpan . funcDecl $ func)
        span
  args' <-
    mapM
        (\((SpanW arg argSpan), (SpanW expectedType expectedTypeSpan)) ->
          do
            argType <- rValueDataType arg
            let (DataType dims argPrimType) = argType
            unless (length dims == 0)
              $ gThrowError --
              $ errTypeNotAllowed
                  [DataType [] TypeInt, DataType [] TypeString]
                  argType
                  argSpan
            unless (argPrimType == expectedType)
              $ gThrowError
              $ errTypeMismatch
                  argType
                  argSpan
                  (DataType [] expectedType)
                  expectedTypeSpan
            return arg
        )
      $ zip args (funcArgTypes . funcDecl $ func)
  return $ RFuncCall funcName args'
 where
  throwFuncNotDeclared sym = case sym of
    Nothing  -> gThrowError $ errFuncNotDeclared funcName span
    Just sym -> return sym

mkExpArithmetic
  :: GrammarM m => SpanW RValue -> OpArithmetic -> SpanW RValue -> m Exp
mkExpArithmetic (SpanW r1 span1) op (SpanW r2 span2) = do
  dataType1 <- rValueDataType r1
  unless (dataType1 == DataType [] TypeInt) $ gThrowError
    (errTypeNotAllowed [DataType [] TypeInt] dataType1 span1)
  dataType2 <- rValueDataType r2
  unless (dataType2 == DataType [] TypeInt) $ gThrowError
    (errTypeNotAllowed [DataType [] TypeInt] dataType2 span2)
  return $ MkExpArithmetic r1 op r2

mkExpRelational
  :: GrammarM m => SpanW RValue -> OpRelational -> SpanW RValue -> m Exp
mkExpRelational (SpanW r1 span1) op (SpanW r2 span2) = do
  let allowedTypes = [DataType [] TypeInt, DataType [] TypeString]

  dataType1 <- rValueDataType r1
  unless (dataType1 `elem` allowedTypes)
    $ gThrowError --
    $ errTypeNotAllowed allowedTypes dataType1 span1

  dataType2 <- rValueDataType r2
  unless (dataType2 `elem` allowedTypes)
    $ gThrowError --
    $ errTypeNotAllowed allowedTypes dataType2 span2

  unless (dataType1 == dataType2)
    $ gThrowError --
    $ errTypeMismatch dataType1 span1 dataType2 span2

  return $ MkExpRelational r1 op r2
mkExpLogical
  :: GrammarM m => SpanW RValue -> OpLogical -> SpanW RValue -> m Exp
mkExpLogical (SpanW r1 span1) op (SpanW r2 span2) = do
  let allowedTypes = [DataType [] TypeBool]

  dataType1 <- rValueDataType r1
  unless (dataType1 `elem` allowedTypes)
    $ gThrowError --
    $ errTypeNotAllowed allowedTypes dataType1 span1

  dataType2 <- rValueDataType r2
  unless (dataType2 `elem` allowedTypes)
    $ gThrowError --
    $ errTypeNotAllowed allowedTypes dataType2 span2

  unless (dataType1 == dataType2)
    $ gThrowError --
    $ errTypeMismatch dataType1 span1 dataType2 span2

  return $ MkExpLogical r1 op r2


doTypeDeclare :: GrammarM m => String -> [(DataType, String, Span)] -> m ()
doTypeDeclare = do
  error "Not implemented"


-- Helper Functions

symLookupCombined :: GrammarM m => String -> m (Maybe Symbol)
symLookupCombined name = do
  fcHasCtxt <- fcHasCtxt
  lSym      <- if fcHasCtxt then fcSymLookup name else return $ Nothing
  case lSym of
    Just sym -> return $ Just sym
    Nothing  -> gSymLookup name

lValueDataType :: GrammarM m => LValue -> m DataType
lValueDataType (LValue indices identName) = do
  (DataType dims primType) <-
    symDataType . fromJust <$> symLookupCombined identName

  let dims' = drop (length indices) dims
  return $ DataType dims' primType

lValueDeclSpan :: GrammarM m => LValue -> m Span
lValueDeclSpan (LValue _ identName) = do
  symDeclSpan . fromJust <$> symLookupCombined identName

rValueDataType :: GrammarM m => RValue -> m DataType
rValueDataType (RLValue v  ) = lValueDataType v
rValueDataType (RExp    exp) = return $ expDataType exp
rValueDataType (RFuncCall funcName _) =
  (DataType []) . funcRetType . funcDecl . fromJust <$> funcLookup funcName

expDataType :: Exp -> DataType
expDataType exp = case exp of
  ExpNum{}          -> DataType [] TypeInt
  ExpStr{}          -> DataType [] TypeString
  MkExpArithmetic{} -> DataType [] TypeInt
  MkExpRelational{} -> DataType [] TypeBool
  MkExpLogical{}    -> DataType [] TypeBool

-- DataType

instance Show PrimitiveType where
  show TypeInt    = "int"
  show TypeString = "str"
  show TypeBool   = "bool"

instance Show DataType where
  show (DataType dims primType) =
    let s = show primType
    in s ++ concatMap (\n -> "[" ++ show n ++ "]") dims

-- Errors

errIdentifierNotDeclared :: String -> Span -> Error
errIdentifierNotDeclared identName span =
  [("Identifier not declared: " ++ identName, span)]

errIdentifierRedeclared :: String -> Span -> Span -> Error
errIdentifierRedeclared identName declSpan span =
  [ ("Identifier redeclared: " ++ identName, span)
  , ("Was already declared here"           , declSpan)
  ]

errTypeMismatch :: DataType -> Span -> DataType -> Span -> Error
errTypeMismatch identDataType declSpan rhsDataType span =
  [ ( "Type mismatch: Expected "
      ++ show identDataType
      ++ ". Got "
      ++ show rhsDataType
    , span
    )
  , ("Was declared here", declSpan)
  ]

errTypeNotAllowed :: [DataType] -> DataType -> Span -> Error
errTypeNotAllowed allowedTypes gotType span =
  [ ( "Type not allowed: "
      ++ show gotType
      ++ ". Allowed types: "
      ++ show allowedTypes
    , span
    )
  ]

errFuncNotDeclared :: String -> Span -> Error
errFuncNotDeclared funcName span =
  [("Function not declared: " ++ funcName, span)]

errFuncRedeclared :: Span -> Span -> Error
errFuncRedeclared span declSpan =
  [("Function redeclared", span), ("Was already declared here", declSpan)]

errFuncRedefined :: Span -> Span -> Error
errFuncRedefined span defSpan =
  [("Function redefined", span), ("Was already defined here", defSpan)]

errFuncArgLengthMismatch :: Int -> Int -> Span -> Span -> Error
errFuncArgLengthMismatch expLength gotLength declSpan span =
  [ ( "Mismatch in function argument length. Expected: "
      ++ show expLength
      ++ ". Got: "
      ++ show gotLength
    , span
    )
  , ("Was declared here", declSpan)
  ]

errFuncDefMismatch :: Span -> Span -> Error
errFuncDefMismatch span declSpan =
  [ ("Function definition and declarations are different", span)
  , ("Was declared here", declSpan)
  ]


errArrayNotAllowed :: Span -> Span -> Error
errArrayNotAllowed span declSpan =
  [ ("An array is not allowed in this position", span)
  , ("Was declared here"                       , declSpan)
  ]
