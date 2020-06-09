{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE DisambiguateRecordFields #-}
{-# LANGUAGE DuplicateRecordFields    #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PatternSynonyms          #-}

module Grammar where

import           Control.Monad (unless)
import           Control.Monad.Except (MonadError,
                                       throwError)
import           Control.Monad.Extra (unlessM)
import           Data.Maybe (fromJust)
import           Error (Error)
import qualified Error
import           Span

newtype Program = Program {stmts :: [Stmt]}

data Stmt
  = StmtAssign StmtAssign
  | StmtRead StmtRead
  | StmtWrite StmtWrite
  | StmtIf StmtIf
  | StmtIfElse StmtIfElse
  | StmtWhile StmtWhile
  | StmtBreak StmtBreak
  | StmtContinue StmtContinue
  | StmtRValue RValue -- For function calls which is both an RValue and Statement
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

data LValue
  = LValue
      [RValue] -- Indices, empty if simple ident
      String
  deriving (Show, Eq)

data RValue = RLValue LValue | RExp Exp | RFuncCall String [RValue] deriving (Show, Eq)

data Exp
  = ExpNum Int
  | ExpStr String
  | MkExpArithmetic RValue OpArithmetic RValue
  | MkExpLogical RValue OpLogical RValue
  deriving (Show, Eq)

data OpArithmetic = OpAdd | OpSub | OpMul | OpDiv | OpMod
  deriving (Show, Eq)

data OpLogical = OpLT | OpGT | OpLE | OpGE | OpEQ | OpNE
  deriving (Show, Eq)

data Symbol
  = Symbol { symName     :: String
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
            , funcArgs      :: [String]
            , funcLocalVars :: [Symbol]
            , funcDefSpan   :: Span
            }

data PrimitiveType = TypeInt | TypeBool | TypeString
  deriving (Eq)

data DataType
  = DataType
      [Int] -- Dims
      PrimitiveType
  deriving (Eq)

doVarDeclare
  :: (MonadError Error m, ReadSymbols m, WriteSymbols m)
  => String
  -> PrimitiveType
  -> [Int]
  -> Span
  -> m ()
doVarDeclare identName primType dims span = do
  symLookup identName >>= throwSymbolExists
  symInsert symbol
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
    Just sym -> throwError
      $ errIdentifierRedeclared identName (symDeclSpan sym) span

mkStmtAssign
  :: (MonadError Error m, ReadSymbols m, ReadFuncs m)
  => LValue
  -> RValue
  -> Span
  -> m StmtAssign
mkStmtAssign lhs rhs span = do
  lhsType     <- lValueDataType lhs
  lhsDeclSpan <- lValueDeclSpan lhs
  rhsType     <- rValueDataType rhs
  let (DataType dims _) = lhsType
  unless (length dims == 0) $ throwError $ errArrayNotAllowed
    span
    lhsDeclSpan
  unless (lhsType == rhsType) $ throwError $ errTypeMismatch
    lhsType
    lhsDeclSpan
    rhsType
    span
  return $ MkStmtAssign lhs rhs

mkStmtRead
  :: (MonadError Error m, ReadSymbols m) => SpanW LValue -> m StmtRead
mkStmtRead (SpanW lValue lValueSpan) = do
  dataType <- lValueDataType lValue
  let allowedTypes = [DataType [] TypeInt, DataType [] TypeString]
  unless (dataType `elem` allowedTypes)
    $ throwError
    $ errTypeNotAllowed allowedTypes dataType lValueSpan
  return $ MkStmtRead lValue

mkStmtWrite
  :: (MonadError Error m, ReadSymbols m, ReadFuncs m)
  => SpanW RValue
  -> m StmtWrite
mkStmtWrite (SpanW rValue rValueSpan) = do
  dataType <- rValueDataType rValue
  let allowedTypes = [DataType [] TypeInt, DataType [] TypeString]
  unless (dataType `elem` allowedTypes)
    $ throwError
    $ errTypeNotAllowed allowedTypes dataType rValueSpan
  return $ MkStmtWrite rValue

mkStmtIf
  :: (MonadError Error m, ReadSymbols m, ReadFuncs m)
  => SpanW RValue
  -> [Stmt]
  -> m StmtIf
mkStmtIf (SpanW cond span) body = do
  dataType <- rValueDataType cond
  unless (dataType == DataType [] TypeBool)
    $ throwError
    $ errTypeNotAllowed [DataType [] TypeBool] dataType span
  return $ MkStmtIf cond body

mkStmtIfElse
  :: (MonadError Error m, ReadSymbols m, ReadFuncs m)
  => SpanW RValue
  -> [Stmt]
  -> [Stmt]
  -> m StmtIfElse
mkStmtIfElse (SpanW cond span) thenBody elseBody = do
  dataType <- rValueDataType cond
  unless (dataType == DataType [] TypeBool)
    $ throwError
    $ errTypeNotAllowed [DataType [] TypeBool] dataType span
  return $ MkStmtIfElse cond thenBody elseBody

mkStmtWhile
  :: (MonadError Error m, ReadSymbols m, ReadFuncs m)
  => SpanW RValue
  -> [Stmt]
  -> m StmtWhile
mkStmtWhile (SpanW cond span) body = do
  dataType <- rValueDataType cond
  unless (dataType == DataType [] TypeBool)
    $ throwError
    $ errTypeNotAllowed [DataType [] TypeBool] dataType span
  return $ MkStmtWhile cond body

mkStmtBreak
  :: (MonadError Error m, ReadLoopStack m) => Span -> m StmtBreak
mkStmtBreak span = do
  unlessM hasLoop throwOutOfLoop
  return $ MkStmtBreak
  where throwOutOfLoop = throwError (Error.syntaxError span)

mkStmtContinue
  :: (MonadError Error m, ReadLoopStack m) => Span -> m StmtContinue
mkStmtContinue span = do
  unlessM hasLoop throwOutOfLoop
  return $ MkStmtContinue
  where throwOutOfLoop = throwError (Error.syntaxError span)

doFuncDeclare
  :: (MonadError Error m, WriteFuncs m)
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
      throwError $ errFuncRedeclared span (funcDeclSpan funcDec)
    Just (FuncDefined funcDec _) ->
      throwError $ errFuncRedeclared span (funcDeclSpan (funcDec))

doFuncDefine
  :: ( MonadError Error m
     , WriteFuncs m
     , WriteSymbols m
     , SymbolTableStack m
     )
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
  symStackPush
  flip mapM_ args $ \(SpanW (name, primType) span) ->
    symInsert $ Symbol
      { symName     = name
      , symDataType = DataType [] primType
      , symDeclSpan = span
      }
  return $ \stmts -> do
    localVars <- symStackPop
    let
      funcDef = FuncDef
        { funcArgs      = fst . spanWVal <$> args
        , funcLocalVars = localVars
        , funcBody      = stmts
        , funcDefSpan   = span
        }
    funcInsert (FuncDefined funcDecl funcDef)
 where
  throwRedefined maybeFunc = case maybeFunc of
    Nothing -> return Nothing
    Just (FuncDefined _ func') ->
      throwError $ errFuncRedefined span (funcDefSpan func')
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
        $ throwError
        $ errFuncDefMismatch span (funcDeclSpan funcDecl)
      return $ Just funcDecl
  declareIfNotDeclared maybeFunc = case maybeFunc of
    Nothing -> do
      doFuncDeclare retType name (fmap (fmap snd) args) span
      func <- funcLookup name
      return $ funcDecl . fromJust $ func
    Just f -> return f

mkLValue
  :: (MonadError Error m, ReadSymbols m, ReadFuncs m)
  => SpanW String
  -> [SpanW RValue]
  -> m LValue
mkLValue (SpanW identName identSpan) indices = do
  sym <- symLookup identName >>= \case
    Nothing ->
      throwError (errIdentifierNotDeclared identName identSpan)
    Just sym -> return sym
  let DataType dims _ = symDataType sym
  unless (length indices <= length dims)
    $ throwError (Error.customError "Too much indices" identSpan) -- TODO Better error
  indices' <- mapM
    (\(SpanW index indexSpan) -> do
      indexType <- rValueDataType index
      unless (indexType == DataType [] TypeInt) $ throwError
        (Error.customError "mkArrayIndex: index not int" indexSpan) -- TODO Better error
      return $ index
    )
    indices
  return $ LValue indices' identName

mkExpFuncCall
  :: (ReadFuncs m, ReadSymbols m, MonadError Error m)
  => String
  -> [SpanW RValue]
  -> Span
  -> m RValue
mkExpFuncCall funcName args span = do
  func <- funcLookup funcName >>= throwFuncNotDeclared
  unless (length args == (length $ funcArgTypes . funcDecl $ func))
    $ throwError
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
            unless (length dims /= 0) $ throwError $ errTypeNotAllowed
              [DataType [] TypeInt, DataType [] TypeString]
              argType
              argSpan
            unless (argPrimType == expectedType)
              $ throwError
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
    Nothing  -> throwError $ errFuncNotDeclared funcName span
    Just sym -> return sym

mkExpArithmetic
  :: (ReadSymbols m, ReadFuncs m, MonadError Error m)
  => SpanW RValue
  -> OpArithmetic
  -> SpanW RValue
  -> m Exp
mkExpArithmetic (SpanW r1 span1) op (SpanW r2 span2) = do
  dataType1 <- rValueDataType r1
  unless (dataType1 == DataType [] TypeInt) $ throwError
    (errTypeNotAllowed [DataType [] TypeInt] dataType1 span1)
  dataType2 <- rValueDataType r2
  unless (dataType2 == DataType [] TypeInt) $ throwError
    (errTypeNotAllowed [DataType [] TypeInt] dataType2 span2)
  return $ MkExpArithmetic r1 op r2

mkExpLogical
  :: (ReadSymbols m, ReadFuncs m, MonadError Error m)
  => SpanW RValue
  -> OpLogical
  -> SpanW RValue
  -> m Exp
mkExpLogical (SpanW r1 span1) op (SpanW r2 span2) = do
  let allowedTypes = [DataType [] TypeInt, DataType [] TypeString]

  dataType1 <- rValueDataType r1
  unless (dataType1 `elem` allowedTypes)
    $ throwError
    $ errTypeNotAllowed allowedTypes dataType1 span1

  dataType2 <- rValueDataType r2
  unless (dataType2 `elem` allowedTypes)
    $ throwError
    $ errTypeNotAllowed allowedTypes dataType2 span2

  unless (dataType1 == dataType2) $ throwError $ errTypeMismatch
    dataType1
    span1
    dataType2
    span2

  return $ MkExpLogical r1 op r2

-- Helper Functions

lValueDataType
  :: (MonadError Error m, ReadSymbols m) => LValue -> m DataType
lValueDataType (LValue indices identName) = do
  (DataType dims primType) <-
    symDataType . fromJust <$> symLookup identName

  let dims' = drop (length indices) dims
  return $ DataType dims' primType

lValueDeclSpan
  :: (MonadError Error m, ReadSymbols m) => LValue -> m Span
lValueDeclSpan (LValue _ identName) = do
  symDeclSpan . fromJust <$> symLookup identName

rValueDataType
  :: (MonadError Error m, ReadSymbols m, ReadFuncs m)
  => RValue
  -> m DataType
rValueDataType (RLValue v  ) = lValueDataType v
rValueDataType (RExp    exp) = return $ expDataType exp
rValueDataType (RFuncCall funcName _) =
  (DataType [])
    .   funcRetType
    .   funcDecl
    .   fromJust
    <$> funcLookup funcName

expDataType :: Exp -> DataType
expDataType exp = case exp of
  ExpNum{}          -> DataType [] TypeInt
  ExpStr{}          -> DataType [] TypeString
  MkExpArithmetic{} -> DataType [] TypeInt
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

-- Typeclasses

class Monad m => ReadSymbols m where
  symLookup :: String -> m (Maybe Symbol)

class Monad m => WriteSymbols m where
  symInsert :: Symbol -> m ()

class Monad m => SymbolTableStack m where
  symStackPush :: m ()
  symStackPop :: m [Symbol]

class Monad m => ReadFuncs m where
  funcLookup :: String -> m (Maybe Func)

class ReadFuncs m => WriteFuncs m where
  funcInsert :: Func -> m ()

class Monad m => ReadLoopStack m where
  hasLoop :: m Bool

class Monad m => LoopStackWriter m where
  pushLoop :: m ()
  popLoop :: m ()

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
  [ ("Function redeclared"      , span)
  , ("Was already declared here", declSpan)
  ]

errFuncRedefined :: Span -> Span -> Error
errFuncRedefined span defSpan =
  [ ("Function redefined"      , span)
  , ("Was already defined here", defSpan)
  ]

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
