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
  , ArrayIndex(..)
  , Exp(..)
  , OpArithmetic(..)
  , OpLogical(..)
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
  , SymbolTableReader(..)
  , SymbolTableWriter(..)
  , SymbolTableRW
  , LoopStackReader(..)
  , LoopStackWriter(..)
  , pushLoopStack
  , popLoopStack
  ) where


import           Control.Error.Safe
import           Flow

import           Error (Error)
import qualified Error
import LoopStack (LoopStack)
import qualified LoopStack
import           Span
import qualified Symbol
import           Symbol (DataType (..), pattern Symbol)
import qualified SymbolTable
import Control.Monad.Except ( MonadError, liftEither, throwError )
import Control.Monad (unless)
import Data.Bifunctor (first)
import Data.Either.Extra (maybeToEither)
import Data.Maybe (fromJust, isJust)

type Symbol = Symbol.Symbol ()
type SymbolTable = SymbolTable.SymbolTable ()

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

data LValue = LValueIdent Ident | LValueArrayIndex ArrayIndex
data RValue = LValue LValue | Exp Exp

data Ident = MkIdent String Span
data ArrayIndex = MkArrayIndex Ident RValue Span

data Exp
  = ExpNum Int Span
  | ExpArithmetic RValue OpArithmetic RValue Span
  | ExpLogical RValue OpLogical RValue Span

data OpArithmetic = OpAdd | OpSub | OpMul | OpDiv | OpMod
data OpLogical = OpLT | OpGT | OpLE | OpGE | OpEQ | OpNE

mkIdent
  :: (MonadError Error m, SymbolTableReader m) => String -> Span -> m Ident
mkIdent name span = do
  symtab <- getSymtab
  unless (isJust (SymbolTable.lookup name symtab))
         (throwError (Error.identifierNotDeclared name span))
  return $ MkIdent name span

mkStmtDeclare
  :: (MonadError Error m, SymbolTableRW m)
  => String
  -> DataType
  -> Span
  -> m StmtDeclare
mkStmtDeclare identName dataType span = do
  symtab  <- getSymtab
  symtab' <- (SymbolTable.insert symbol symtab |> throwSymbolExists)
  putSymtab symtab'
  return $ MkStmtDeclare identName dataType span
 where
  symbol = Symbol { Symbol.name     = identName
                  , Symbol.declSpan = span
                  , Symbol.dataType = dataType
                  , Symbol.ext      = ()
                  }
  throwSymbolExists = liftEither . first
    (\(SymbolTable.SymbolExists symbol1) ->
      Error.identifierRedeclared identName (Symbol.declSpan symbol1) span
    )

mkStmtAssign
  :: (MonadError Error m, SymbolTableReader m)
  => LValue
  -> RValue
  -> Span
  -> m StmtAssign
mkStmtAssign lhs rhs span = do
  lhsType     <- lValueDataType lhs
  lhsDeclSpan <- lValueDeclSpan lhs
  rhsType     <- rValueDataType rhs
  unless (lhsType == rhsType) $ throwError $ Error.assignmentTypeMismatch
    lhsType
    lhsDeclSpan
    rhsType
    span
  return $ MkStmtAssign lhs rhs span

mkStmtRead
  :: (MonadError Error m, SymbolTableReader m) => LValue -> Span -> m StmtRead
mkStmtRead lhs span = do
  dataType <- lValueDataType lhs
  unless (dataType == Symbol.DataTypeInt) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeInt]
    dataType
    span
  return $ MkStmtRead lhs span

mkStmtWrite
  :: (MonadError Error m, SymbolTableReader m) => RValue -> Span -> m StmtWrite
mkStmtWrite exp span = do
  dataType <- rValueDataType exp
  unless (dataType == Symbol.DataTypeInt) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeInt]
    dataType
    span
  return $ MkStmtWrite exp span

mkStmtIf
  :: (MonadError Error m, SymbolTableReader m)
  => RValue
  -> [Stmt]
  -> Span
  -> m StmtIf
mkStmtIf exp body span = do
  dataType <- rValueDataType exp
  unless (dataType == Symbol.DataTypeBool) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeBool]
    dataType
    span
  return $ MkStmtIf exp body span

mkStmtIfElse
  :: (MonadError Error m, SymbolTableReader m)
  => RValue
  -> [Stmt]
  -> [Stmt]
  -> Span
  -> m StmtIfElse
mkStmtIfElse exp thenBody elseBody span = do
  dataType <- rValueDataType exp
  unless (dataType == Symbol.DataTypeBool) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeBool]
    dataType
    span
  return $ MkStmtIfElse exp thenBody elseBody span

mkStmtWhile
  :: (MonadError Error m, SymbolTableReader m)
  => RValue
  -> [Stmt]
  -> Span
  -> m StmtWhile
mkStmtWhile exp body span = do
  dataType <- rValueDataType exp
  unless (dataType == Symbol.DataTypeBool) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeBool]
    dataType
    span
  return $ MkStmtWhile exp body span

mkStmtDoWhile
  :: (MonadError Error m, SymbolTableReader m)
  => RValue
  -> [Stmt]
  -> Span
  -> m StmtDoWhile
mkStmtDoWhile exp body span = do
  dataType <- rValueDataType exp
  unless (dataType == Symbol.DataTypeBool) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeBool]
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

mkArrayIndex
  :: (MonadError Error m, SymbolTableReader m)
  => Ident
  -> RValue
  -> Span
  -> m ArrayIndex
mkArrayIndex ident index span = do
  dataType <- identDataType ident
  unless
      (case dataType of
        DataTypeArray _ _ -> True
        _                 -> False
      )
    $ throwError (Error.syntaxError span) -- TODO Better error
  indexType <- rValueDataType index
  unless (indexType == DataTypeInt) $ throwError (Error.syntaxError span) -- TODO Better error
  return $ MkArrayIndex ident index span

-- Helper Functions

rValueDataType
  :: (MonadError Error m, SymbolTableReader m) => RValue -> m DataType
rValueDataType (LValue v  ) = lValueDataType v
rValueDataType (Exp    exp) = return $ expDataType exp

lValueDataType
  :: (MonadError Error m, SymbolTableReader m) => LValue -> m DataType
lValueDataType lValue = case lValue of
  (LValueArrayIndex lValue) -> do
    let (MkArrayIndex ident _ _) = lValue
    dataType <- identDataType ident
    let (DataTypeArray _ innerType) = dataType
    return innerType
  (LValueIdent ident) -> identDataType ident

lValueDeclSpan :: (SymbolTableReader m) => LValue -> m Span
lValueDeclSpan lValue =
  let ident = case lValue of
        LValueIdent      ident                    -> ident
        LValueArrayIndex (MkArrayIndex ident _ _) -> ident
  in  Symbol.declSpan <$> identSymbol ident

identSymbol :: (SymbolTableReader m) => Ident -> m Symbol
identSymbol ident = do
  let (MkIdent identName _) = ident
  fromJust . SymbolTable.lookup identName <$> getSymtab

identDataType
  :: (MonadError Error m, SymbolTableReader m) => Ident -> m DataType
identDataType ident = Symbol.dataType <$> identSymbol ident

expDataType :: Exp -> DataType
expDataType exp = case exp of
  ExpNum{}        -> DataTypeInt
  ExpArithmetic{} -> DataTypeInt
  ExpLogical{}    -> DataTypeBool

-- HasSpan instances

instance HasSpan Ident where
  getSpan (MkIdent _ span) = span

instance HasSpan RValue where
  getSpan exp = case exp of
    LValue v   -> getSpan v
    Exp    exp -> getSpan exp

instance HasSpan LValue where
  getSpan exp = case exp of
    LValueIdent      ident  -> getSpan ident
    LValueArrayIndex aindex -> getSpan aindex

instance HasSpan ArrayIndex where
  getSpan exp = case exp of
    MkArrayIndex _ _ span -> span

instance HasSpan Exp where
  getSpan exp = case exp of
    ExpNum _ span            -> span
    ExpArithmetic _ _ _ span -> span
    ExpLogical    _ _ _ span -> span

-- Typeclasses

class Monad m => SymbolTableReader m where
  getSymtab :: m SymbolTable

class Monad m => SymbolTableWriter m where
  putSymtab :: SymbolTable -> m ()

type SymbolTableRW m = (SymbolTableReader m, SymbolTableWriter m)

class Monad m => LoopStackReader m where
  getLoopStack :: m LoopStack

class Monad m => LoopStackWriter m where
  putLoopStack :: LoopStack -> m ()

pushLoopStack :: (LoopStackReader m, LoopStackWriter m) => m ()
pushLoopStack = getLoopStack >>= (putLoopStack . LoopStack.push)

popLoopStack :: (LoopStackReader m, LoopStackWriter m) => m ()
popLoopStack = getLoopStack >>= (putLoopStack . fromJust . LoopStack.pop)
