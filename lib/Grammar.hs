{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Grammar where

import           Flow

import           Error (Error)
import qualified Error
import LoopStack (LoopStack)
import qualified LoopStack
import           Span
import Control.Monad.Except ( MonadError, liftEither, throwError )
import Control.Monad (unless)
import Control.Monad.Extra (unlessM)
import Data.Bifunctor (first)
import Data.Either.Extra (maybeToEither)
import Data.Maybe (fromJust, isJust)

newtype Program = Program { stmts :: [Stmt] }

data Stmt
  = StmtDeclare StmtDeclare
  | StmtAssign StmtAssign
  | StmtRead StmtRead
  | StmtWrite StmtWrite
  | StmtIf StmtIf
  | StmtIfElse StmtIfElse
  | StmtWhile StmtWhile
  | StmtDoWhile StmtDoWhile
  | StmtBreak StmtBreak
  | StmtContinue StmtContinue

data StmtDeclare = MkStmtDeclare String DataType
data StmtAssign = MkStmtAssign LValue RValue
data StmtRead = MkStmtRead LValue
data StmtWrite = MkStmtWrite RValue
data StmtIf = MkStmtIf RValue [Stmt]
data StmtIfElse = MkStmtIfElse RValue [Stmt] [Stmt]
data StmtWhile = MkStmtWhile RValue [Stmt]
data StmtDoWhile = MkStmtDoWhile RValue [Stmt]
data StmtBreak = MkStmtBreak
data StmtContinue = MkStmtContinue

data LValue = LValue
                [RValue] -- Indices, empty if simple ident
                Ident
data RValue = RLValue LValue | RExp Exp

data Ident = MkIdent String deriving (Show)

data Exp
  = ExpNum Int
  | ExpStr String
  | MkExpArithmetic RValue OpArithmetic RValue
  | MkExpLogical RValue OpLogical RValue

data OpArithmetic = OpAdd | OpSub | OpMul | OpDiv | OpMod
data OpLogical = OpLT | OpGT | OpLE | OpGE | OpEQ | OpNE

data Symbol =
  Symbol
    { symName :: String
    , symDataType :: DataType
    , symDeclSpan :: Span
    }
    deriving Show

data PrimitiveType = TypeInt | TypeBool | TypeString
  deriving (Eq, Show)

data DataType
  = DataType
      [Int] -- Dims
      PrimitiveType
  deriving Eq

mkIdent :: (MonadError Error m, ReadSymbols m) => SpanW String -> m Ident
mkIdent (SpanW name span) = do
  unlessM (isJust <$> symLookup name)
          (throwError (errIdentifierNotDeclared name span))
  return $ MkIdent name

mkStmtDeclare
  :: (MonadError Error m, WriteSymbols m)
  => String
  -> PrimitiveType
  -> [Int]
  -> Span
  -> m StmtDeclare
mkStmtDeclare identName primType dims span = do
  symInsert symbol >>= throwSymbolExists
  return $ MkStmtDeclare identName dataType
 where
  dataType = DataType dims primType
  symbol =
    Symbol { symName = identName, symDeclSpan = span, symDataType = dataType }
  throwSymbolExists = liftEither . first
    (\(SymbolExists symbol1) ->
      errIdentifierRedeclared identName (symDeclSpan symbol1) span
    )

mkStmtAssign
  :: (MonadError Error m, ReadSymbols m)
  => LValue
  -> RValue
  -> Span
  -> m StmtAssign
mkStmtAssign lhs rhs span = do
  lhsType     <- lValueDataType lhs
  lhsDeclSpan <- lValueDeclSpan lhs
  rhsType     <- rValueDataType rhs
  unless (lhsType == rhsType) $ throwError $ errAssignmentTypeMismatch
    lhsType
    lhsDeclSpan
    rhsType
    span
  let
  return $ MkStmtAssign lhs rhs

mkStmtRead :: (MonadError Error m, ReadSymbols m) => SpanW LValue -> m StmtRead
mkStmtRead (SpanW lValue lValueSpan) = do
  dataType <- lValueDataType lValue
  let allowedTypes = [DataType [] TypeInt, DataType [] TypeString]
  unless (dataType `elem` allowedTypes) $ throwError $ errTypeNotAllowed
    allowedTypes
    dataType
    lValueSpan
  return $ MkStmtRead lValue

mkStmtWrite
  :: (MonadError Error m, ReadSymbols m) => SpanW RValue -> m StmtWrite
mkStmtWrite (SpanW rValue rValueSpan) = do
  dataType <- rValueDataType rValue
  let allowedTypes = [DataType [] TypeInt, DataType [] TypeString]
  unless (dataType `elem` allowedTypes) $ throwError $ errTypeNotAllowed
    allowedTypes
    dataType
    rValueSpan
  return $ MkStmtWrite rValue

mkStmtIf
  :: (MonadError Error m, ReadSymbols m) => SpanW RValue -> [Stmt] -> m StmtIf
mkStmtIf (SpanW cond span) body = do
  dataType <- rValueDataType cond
  unless (dataType == DataType [] TypeBool) $ throwError $ errTypeNotAllowed
    [DataType [] TypeBool]
    dataType
    span
  return $ MkStmtIf cond body

mkStmtIfElse
  :: (MonadError Error m, ReadSymbols m)
  => SpanW RValue
  -> [Stmt]
  -> [Stmt]
  -> m StmtIfElse
mkStmtIfElse (SpanW cond span) thenBody elseBody = do
  dataType <- rValueDataType cond
  unless (dataType == DataType [] TypeBool) $ throwError $ errTypeNotAllowed
    [DataType [] TypeBool]
    dataType
    span
  return $ MkStmtIfElse cond thenBody elseBody

mkStmtWhile
  :: (MonadError Error m, ReadSymbols m)
  => SpanW RValue
  -> [Stmt]
  -> m StmtWhile
mkStmtWhile (SpanW cond span) body = do
  dataType <- rValueDataType cond
  unless (dataType == DataType [] TypeBool) $ throwError $ errTypeNotAllowed
    [DataType [] TypeBool]
    dataType
    span
  return $ MkStmtWhile cond body

mkStmtDoWhile
  :: (MonadError Error m, ReadSymbols m)
  => SpanW RValue
  -> [Stmt]
  -> m StmtDoWhile
mkStmtDoWhile (SpanW cond span) body = do
  dataType <- rValueDataType cond
  unless (dataType == DataType [] TypeBool) $ throwError $ errTypeNotAllowed
    [DataType [] TypeBool]
    dataType
    span
  return $ MkStmtDoWhile cond body

mkStmtBreak :: (MonadError Error m, LoopStackReader m) => Span -> m StmtBreak
mkStmtBreak span = do
  loopStack <- getLoopStack
  _         <- LoopStack.pop loopStack |> throwOutOfLoop
  return $ MkStmtBreak
  where throwOutOfLoop = liftEither . maybeToEither (Error.syntaxError span)

mkStmtContinue
  :: (MonadError Error m, LoopStackReader m) => Span -> m StmtContinue
mkStmtContinue span = do
  loopStack <- getLoopStack
  _         <- LoopStack.pop loopStack |> throwOutOfLoop
  return $ MkStmtContinue
  where throwOutOfLoop = liftEither . maybeToEither (Error.syntaxError span)

mkLValue
  :: (MonadError Error m, ReadSymbols m)
  => SpanW Ident
  -> [SpanW RValue]
  -> m LValue
mkLValue (SpanW ident identSpan) indices = do
  DataType dims _ <- identDataType ident
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
  return $ LValue indices' ident

mkExpArithmetic
  :: (ReadSymbols m, MonadError Error m)
  => SpanW RValue
  -> OpArithmetic
  -> SpanW RValue
  -> m Exp
mkExpArithmetic (SpanW r1 span1) op (SpanW r2 span2) = do
  dataType1 <- rValueDataType r1
  unless (dataType1 == DataType [] TypeInt)
    $ throwError (errTypeNotAllowed [DataType [] TypeInt] dataType1 span1)
  dataType2 <- rValueDataType r2
  unless (dataType2 == DataType [] TypeInt)
    $ throwError (errTypeNotAllowed [DataType [] TypeInt] dataType2 span2)
  return $ MkExpArithmetic r1 op r2

mkExpLogical
  :: (ReadSymbols m, MonadError Error m)
  => SpanW RValue
  -> OpLogical
  -> SpanW RValue
  -> m Exp
mkExpLogical (SpanW r1 span1) op (SpanW r2 span2) = do
  dataType1 <- rValueDataType r1
  let allowedTypes = [DataType [] TypeInt, DataType [] TypeString]
  unless (dataType1 `elem` allowedTypes) $ throwError $ errTypeNotAllowed
    allowedTypes
    dataType1
    span1

  dataType2 <- rValueDataType r2
  unless (dataType2 `elem` allowedTypes) $ throwError $ errTypeNotAllowed
    allowedTypes
    dataType2
    span2

  return $ MkExpLogical r1 op r2

-- Helper Functions
identDataType (MkIdent identName) =
  symDataType . fromJust <$> symLookup identName

lValueDataType (LValue indices ident) = do
  (DataType dims primType) <- identDataType ident
  let dims' = drop (length indices) dims
  return $ DataType dims' primType

lValueDeclSpan (LValue _ ident) = do
  let (MkIdent identName) = ident
  symDeclSpan . fromJust <$> symLookup identName

rValueDataType :: (MonadError Error m, ReadSymbols m) => RValue -> m DataType
rValueDataType (RLValue v  ) = lValueDataType v
rValueDataType (RExp    exp) = return $ expDataType exp

expDataType :: Exp -> DataType
expDataType exp = case exp of
  ExpNum{}          -> DataType [] TypeInt
  ExpStr{}          -> DataType [] TypeString
  MkExpArithmetic{} -> DataType [] TypeInt
  MkExpLogical{}    -> DataType [] TypeBool

-- DataType

instance Show DataType where
  show (DataType dims primType) =
    let s = show primType in s ++ concatMap (\n -> "[" ++ show n ++ "]") dims

-- Typeclasses

class Monad m => ReadSymbols m where
  symLookup :: String -> m (Maybe Symbol)

class ReadSymbols m => WriteSymbols m where
  symInsert :: Symbol ->  m (Either (SymbolExists) ())

data SymbolExists = SymbolExists Symbol

class Monad m => LoopStackReader m where
  getLoopStack :: m LoopStack

class Monad m => LoopStackWriter m where
  putLoopStack :: LoopStack -> m ()

pushLoopStack :: (LoopStackReader m, LoopStackWriter m) => m ()
pushLoopStack = getLoopStack >>= (putLoopStack . LoopStack.push)

popLoopStack :: (LoopStackReader m, LoopStackWriter m) => m ()
popLoopStack = getLoopStack >>= (putLoopStack . fromJust . LoopStack.pop)


-- Errors

errIdentifierNotDeclared :: String -> Span -> Error
errIdentifierNotDeclared identName span =
  [("Identifier not declared: " ++ identName, span)]

errIdentifierRedeclared :: String -> Span -> Span -> Error
errIdentifierRedeclared identName declSpan span =
  [ ("Identifier redeclared: " ++ identName, span)
  , ("Was already declared here"           , declSpan)
  ]

-- errIdentifierNotInitialized :: String -> Span -> Span -> Error
-- errIdentifierNotInitialized identName declSpan span =
--   [ ("Identifier not initialized: " ++ identName, span)
--   , ("Was declared here"                        , declSpan)
--   ]

errAssignmentTypeMismatch :: DataType -> Span -> DataType -> Span -> Error
errAssignmentTypeMismatch identDataType declSpan rhsDataType span =
  [ ( "Assignment type mismatch - "
      ++ show identDataType
      ++ " = "
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
