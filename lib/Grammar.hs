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

data StmtDeclare = MkStmtDeclare String DataType Span
data StmtAssign = MkStmtAssign LValue RValue Span
data StmtRead = MkStmtRead LValue Span
data StmtWrite = MkStmtWrite RValue Span
data StmtIf = MkStmtIf RValue [Stmt] Span
data StmtIfElse = MkStmtIfElse RValue [Stmt] [Stmt] Span
data StmtWhile = MkStmtWhile RValue [Stmt] Span
data StmtDoWhile = MkStmtDoWhile RValue [Stmt] Span
data StmtBreak = MkStmtBreak Span
data StmtContinue = MkStmtContinue Span

data LValue = LValueIdent Ident | LValueArrayIndex RValue LValue Span
data RValue = LValue LValue | Exp Exp

data Ident = MkIdent String Span deriving (Show)

data Exp
  = ExpNum Int Span
  | ExpArithmetic RValue OpArithmetic RValue Span
  | ExpLogical RValue OpLogical RValue Span

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

mkIdent :: (MonadError Error m, ReadSymbols m) => String -> Span -> m Ident
mkIdent name span = do
  unlessM (isJust <$> symLookup name)
          (throwError (errIdentifierNotDeclared name span))
  return $ MkIdent name span

mkStmtDeclare
  :: (MonadError Error m, WriteSymbols m)
  => String
  -> DataType
  -> Span
  -> m StmtDeclare
mkStmtDeclare identName dataType span = do
  symInsert symbol >>= throwSymbolExists
  return $ MkStmtDeclare identName dataType span
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
  return $ MkStmtAssign lhs rhs span

mkStmtRead
  :: (MonadError Error m, ReadSymbols m) => LValue -> Span -> m StmtRead
mkStmtRead lhs span = do
  dataType <- lValueDataType lhs
  unless (dataType == DataTypeInt) $ throwError $ errTypeNotAllowed
    [DataTypeInt]
    dataType
    span
  return $ MkStmtRead lhs span

mkStmtWrite
  :: (MonadError Error m, ReadSymbols m) => RValue -> Span -> m StmtWrite
mkStmtWrite exp span = do
  dataType <- rValueDataType exp
  unless (dataType == DataTypeInt) $ throwError $ errTypeNotAllowed
    [DataTypeInt]
    dataType
    span
  return $ MkStmtWrite exp span

mkStmtIf
  :: (MonadError Error m, ReadSymbols m) => RValue -> [Stmt] -> Span -> m StmtIf
mkStmtIf exp body span = do
  dataType <- rValueDataType exp
  unless (dataType == DataTypeBool) $ throwError $ errTypeNotAllowed
    [DataTypeBool]
    dataType
    span
  return $ MkStmtIf exp body span

mkStmtIfElse
  :: (MonadError Error m, ReadSymbols m)
  => RValue
  -> [Stmt]
  -> [Stmt]
  -> Span
  -> m StmtIfElse
mkStmtIfElse exp thenBody elseBody span = do
  dataType <- rValueDataType exp
  unless (dataType == DataTypeBool) $ throwError $ errTypeNotAllowed
    [DataTypeBool]
    dataType
    span
  return $ MkStmtIfElse exp thenBody elseBody span

mkStmtWhile
  :: (MonadError Error m, ReadSymbols m)
  => RValue
  -> [Stmt]
  -> Span
  -> m StmtWhile
mkStmtWhile exp body span = do
  dataType <- rValueDataType exp
  unless (dataType == DataTypeBool) $ throwError $ errTypeNotAllowed
    [DataTypeBool]
    dataType
    span
  return $ MkStmtWhile exp body span

mkStmtDoWhile
  :: (MonadError Error m, ReadSymbols m)
  => RValue
  -> [Stmt]
  -> Span
  -> m StmtDoWhile
mkStmtDoWhile exp body span = do
  dataType <- rValueDataType exp
  unless (dataType == DataTypeBool) $ throwError $ errTypeNotAllowed
    [DataTypeBool]
    dataType
    span
  return $ MkStmtDoWhile exp body span

mkStmtBreak :: (MonadError Error m, LoopStackReader m) => Span -> m StmtBreak
mkStmtBreak span = do
  loopStack <- getLoopStack
  _         <- LoopStack.pop loopStack |> throwOutOfLoop
  return $ MkStmtBreak span
  where throwOutOfLoop = liftEither . maybeToEither (Error.syntaxError span)

mkStmtContinue
  :: (MonadError Error m, LoopStackReader m) => Span -> m StmtContinue
mkStmtContinue span = do
  loopStack <- getLoopStack
  _         <- LoopStack.pop loopStack |> throwOutOfLoop
  return $ MkStmtContinue span
  where throwOutOfLoop = liftEither . maybeToEither (Error.syntaxError span)


{-
  int foo[m][n] -> Arr m (Arr n Int)
  foo[x][y] -> Idx x (Idx y foo)
-}

-- TODO - !!! Fix erroring out on index
mkArrayIndex
  :: (MonadError Error m, ReadSymbols m) => LValue -> RValue -> Span -> m LValue
mkArrayIndex lValue index span = do
  dataType <- lValueDataType lValue
  unless
      (case dataType of
        DataTypeArray _ _ -> True
        _                 -> False
      )
    $ throwError (Error.customError "mkArrayIndex: LValue not an array" span) -- TODO Better error
  indexType <- rValueDataType index
  unless (indexType == DataTypeInt)
    $ throwError (Error.customError "mkArrayIndex: RValue not Int" span) -- TODO Better error
  return $ LValueArrayIndex index lValue span

-- Helper Functions

lValueIdent :: LValue -> Ident
lValueIdent (LValueIdent ident          ) = ident
lValueIdent (LValueArrayIndex _ lValue _) = lValueIdent lValue

rValueDataType :: (MonadError Error m, ReadSymbols m) => RValue -> m DataType
rValueDataType (LValue v  ) = lValueDataType v
rValueDataType (Exp    exp) = return $ expDataType exp

lValueDataType :: (MonadError Error m, ReadSymbols m) => LValue -> m DataType
lValueDataType lValue = case lValue of
  (LValueArrayIndex _ lValueInner _) -> do
    dataType <- lValueDataType lValueInner
    return $ dataTypeRemoveInner dataType
  (LValueIdent (MkIdent identName _)) ->
    symDataType . fromJust <$> symLookup identName

dataTypeRemoveInner :: DataType -> DataType
dataTypeRemoveInner dataType = case dataType of
  DataTypeArray _dim innerDataType -> case innerDataType of
    DataTypeArray{} -> dataTypeRemoveInner innerDataType
    _               -> innerDataType
  _ -> error "removeInner: not DataTypeArray"


lValueDeclSpan :: (ReadSymbols m) => LValue -> m Span
lValueDeclSpan lValue = case lValue of
  LValueIdent (MkIdent identName _) ->
    symDeclSpan . fromJust <$> symLookup identName
  LValueArrayIndex _ lValueInner _ -> lValueDeclSpan lValueInner


expDataType :: Exp -> DataType
expDataType exp = case exp of
  ExpNum{}        -> DataTypeInt
  ExpArithmetic{} -> DataTypeInt
  ExpLogical{}    -> DataTypeBool

-- HasSpan instances

instance HasSpan RValue where
  getSpan exp = case exp of
    LValue v   -> getSpan v
    Exp    exp -> getSpan exp

instance HasSpan LValue where
  getSpan exp = case exp of
    LValueIdent (MkIdent _ span) -> span
    LValueArrayIndex _ _ span    -> span

instance HasSpan Exp where
  getSpan exp = case exp of
    ExpNum _ span            -> span
    ExpArithmetic _ _ _ span -> span
    ExpLogical    _ _ _ span -> span

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
