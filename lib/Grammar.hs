{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Grammar
  ( Program(..)
  , Stmt(..)
  , StmtDeclare(..)
  , StmtAssign(..)
  , StmtRead(..)
  , StmtWrite(..)
  , StmtIf(..)
  , StmtIfElse(..)
  , StmtWhile(..)
  , StmtDoWhile(..)
  , StmtBreak(..)
  , StmtContinue(..)
  , LValue(..)
  , RValue(..)
  , Ident(..)
  , Exp(..)
  , OpArithmetic(..)
  , OpLogical(..)
  , Symbol(..)
  , DataType(..)
  , mkIdent
  , mkStmtDeclare
  , mkStmtAssign
  , mkStmtRead
  , mkStmtWrite
  , mkStmtIf
  , mkStmtIfElse
  , mkStmtWhile
  , mkStmtDoWhile
  , mkStmtBreak
  , mkStmtContinue
  , mkArrayIndex
  , mkExpArithmetic
  , mkExpLogical
  , ReadSymbols(..)
  , WriteSymbols(..)
  , SymbolExists(..)
  , LoopStackReader(..)
  , LoopStackWriter(..)
  , pushLoopStack
  , popLoopStack
  , lValueIdent
  , dataTypeSize
  ) where


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

data LValue = LValueIdent Ident | LValueArrayIndex RValue LValue
data RValue = LValue LValue | Exp Exp

data Ident = MkIdent String deriving (Show)

data Exp
  = ExpNum Int
  | ExpArithmetic RValue OpArithmetic RValue
  | ExpLogical RValue OpLogical RValue

data OpArithmetic = OpAdd | OpSub | OpMul | OpDiv | OpMod
data OpLogical = OpLT | OpGT | OpLE | OpGE | OpEQ | OpNE

data Symbol =
  Symbol
    { symName :: String
    , symDataType :: DataType
    , symDeclSpan :: Span
    }
    deriving Show

data DataType
  = DataTypeInt
  | DataTypeBool
  | DataTypeArray Int DataType
  deriving Eq

mkIdent :: (MonadError Error m, ReadSymbols m) => SpanW String -> m Ident
mkIdent (SpanW name span) = do
  unlessM (isJust <$> symLookup name)
          (throwError (errIdentifierNotDeclared name span))
  return $ MkIdent name

mkStmtDeclare
  :: (MonadError Error m, WriteSymbols m)
  => String
  -> DataType
  -> Span
  -> m StmtDeclare
mkStmtDeclare identName dataType span = do
  symInsert symbol >>= throwSymbolExists
  return $ MkStmtDeclare identName dataType
 where
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
  return $ MkStmtAssign lhs rhs

mkStmtRead :: (MonadError Error m, ReadSymbols m) => SpanW LValue -> m StmtRead
mkStmtRead (SpanW lValue lValueSpan) = do
  dataType <- lValueDataType lValue
  unless (dataType == DataTypeInt) $ throwError $ errTypeNotAllowed
    [DataTypeInt]
    dataType
    lValueSpan
  return $ MkStmtRead lValue

mkStmtWrite
  :: (MonadError Error m, ReadSymbols m) => SpanW RValue -> m StmtWrite
mkStmtWrite (SpanW rValue rValueSpan) = do
  dataType <- rValueDataType rValue
  unless (dataType == DataTypeInt) $ throwError $ errTypeNotAllowed
    [DataTypeInt]
    dataType
    rValueSpan
  return $ MkStmtWrite rValue

mkStmtIf
  :: (MonadError Error m, ReadSymbols m) => SpanW RValue -> [Stmt] -> m StmtIf
mkStmtIf (SpanW cond span) body = do
  dataType <- rValueDataType cond
  unless (dataType == DataTypeBool) $ throwError $ errTypeNotAllowed
    [DataTypeBool]
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
  unless (dataType == DataTypeBool) $ throwError $ errTypeNotAllowed
    [DataTypeBool]
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
  unless (dataType == DataTypeBool) $ throwError $ errTypeNotAllowed
    [DataTypeBool]
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
  unless (dataType == DataTypeBool) $ throwError $ errTypeNotAllowed
    [DataTypeBool]
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


-- TODO - !!! Fix erroring out on index
mkArrayIndex
  :: (MonadError Error m, ReadSymbols m)
  => SpanW LValue
  -> SpanW RValue
  -> m LValue
mkArrayIndex (SpanW lValue lValueSpan) (SpanW index indexSpan) = do
  -- int foo[m][n] -> Arr m (Arr n Int)
  -- foo[x][y] -> Idx x (Idx y foo)
  dataType <- lValueDataType lValue
  unless
      (case dataType of
        DataTypeArray _ _ -> True
        _                 -> False
      )
    $ throwError
        (Error.customError "mkArrayIndex: LValue not an array" lValueSpan) -- TODO Better error
  indexType <- rValueDataType index
  unless (indexType == DataTypeInt)
    $ throwError (Error.customError "mkArrayIndex: index not int" indexSpan) -- TODO Better error
  return $ LValueArrayIndex index lValue

mkExpArithmetic
  :: (ReadSymbols m, MonadError Error m)
  => SpanW RValue
  -> OpArithmetic
  -> SpanW RValue
  -> m Exp
mkExpArithmetic (SpanW r1 span1) op (SpanW r2 span2) = do
  dataType1 <- rValueDataType r1
  unless (dataType1 == DataTypeInt)
    $ throwError (errTypeNotAllowed [DataTypeInt] dataType1 span1)
  dataType2 <- rValueDataType r2
  unless (dataType2 == DataTypeInt)
    $ throwError (errTypeNotAllowed [DataTypeInt] dataType2 span2)
  return $ ExpArithmetic r1 op r2

mkExpLogical
  :: (ReadSymbols m, MonadError Error m)
  => SpanW RValue
  -> OpLogical
  -> SpanW RValue
  -> m Exp
mkExpLogical (SpanW r1 span1) op (SpanW r2 span2) = do
  dataType1 <- rValueDataType r1
  unless (dataType1 == DataTypeInt)
    $ throwError (errTypeNotAllowed [DataTypeInt] dataType1 span1)
  dataType2 <- rValueDataType r2
  unless (dataType2 == DataTypeInt)
    $ throwError (errTypeNotAllowed [DataTypeInt] dataType2 span2)
  return $ ExpLogical r1 op r2

-- Helper Functions

lValueIdent :: LValue -> Ident
lValueIdent (LValueIdent ident        ) = ident
lValueIdent (LValueArrayIndex _ lValue) = lValueIdent lValue

rValueDataType :: (MonadError Error m, ReadSymbols m) => RValue -> m DataType
rValueDataType (LValue v  ) = lValueDataType v
rValueDataType (Exp    exp) = return $ expDataType exp

lValueDataType :: (MonadError Error m, ReadSymbols m) => LValue -> m DataType
lValueDataType lValue = case lValue of
  (LValueArrayIndex _ lValueInner) -> do
    dataType <- lValueDataType lValueInner
    return $ dataTypeRemoveInner dataType
  (LValueIdent (MkIdent identName)) ->
    symDataType . fromJust <$> symLookup identName

dataTypeRemoveInner :: DataType -> DataType
dataTypeRemoveInner dataType = case dataType of
  DataTypeArray _dim innerDataType -> case innerDataType of
    DataTypeArray{} -> dataTypeRemoveInner innerDataType
    _               -> innerDataType
  _ -> error "removeInner: not DataTypeArray"


lValueDeclSpan :: (ReadSymbols m) => LValue -> m Span
lValueDeclSpan lValue = case lValue of
  LValueIdent (MkIdent identName) ->
    symDeclSpan . fromJust <$> symLookup identName
  LValueArrayIndex _ lValueInner -> lValueDeclSpan lValueInner


expDataType :: Exp -> DataType
expDataType exp = case exp of
  ExpNum{}        -> DataTypeInt
  ExpArithmetic{} -> DataTypeInt
  ExpLogical{}    -> DataTypeBool

-- DataType

instance Show DataType where
  show dataType =
    let (s, dims) = dataTypeDims dataType
    in  s ++ concatMap (\n -> "[" ++ show n ++ "]") dims

dataTypeDims dataType = case dataType of
  DataTypeInt  -> ("int", [])
  DataTypeBool -> ("bool", [])
  DataTypeArray dim inner ->
    let (s, dims) = dataTypeDims inner in (s, dim : dims)

dataTypeSize :: DataType -> Int
dataTypeSize DataTypeInt                = 1
dataTypeSize DataTypeBool               = 1
dataTypeSize (DataTypeArray size inner) = size * dataTypeSize inner

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
