{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE PatternSynonyms #-}

module Grammar (
  Program(..),
  Ident(..), mkIdent,
  Stmt(..),
  StmtDeclare(..), mkStmtDeclare,
  StmtAssign(..), mkStmtAssign,
  StmtRead(..), mkStmtRead,
  StmtWrite(..), mkStmtWrite,
  StmtIf(..), mkStmtIf,
  StmtIfElse(..), mkStmtIfElse,
  StmtWhile(..), mkStmtWhile,
  pushLoopStack, popLoopStack,
  StmtDoWhile(..), mkStmtDoWhile,
  StmtBreak(..), mkStmtBreak,
  StmtContinue(..), mkStmtContinue,
  Exp(..),
  OpArithmetic(..),
  OpLogical(..),
  ExpIdent(..),
  ExpPure(..),
  SymbolTableReader(..), SymbolTableWriter(..), SymbolTableRW,
  LoopStackReader(..), LoopStackWriter(..)
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

data Ident = MkIdent String Span

mkIdent
  :: (MonadError Error m, SymbolTableReader m) => String -> Span -> m Ident
mkIdent name span = do
  symtab <- getSymtab
  unless (isJust (SymbolTable.lookup name symtab))
         (throwError (Error.identifierNotDeclared name span))
  return $ MkIdent name span

data StmtDeclare = MkStmtDeclare String DataType Span

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

data StmtAssign = MkStmtAssign Ident Exp Span

mkStmtAssign
  :: (MonadError Error m, SymbolTableReader m)
  => Ident
  -> Exp
  -> Span
  -> m StmtAssign
mkStmtAssign ident exp span = do
  symbol <- identSymbol ident
  let lhsType = Symbol.dataType symbol
  rhsType <- expDataType exp
  unless (lhsType == rhsType) $ throwError $ Error.assignmentTypeMismatch
    lhsType
    (Symbol.declSpan symbol)
    rhsType
    span
  return $ MkStmtAssign ident exp span

data StmtRead = MkStmtRead Ident Span

mkStmtRead
  :: (MonadError Error m, SymbolTableReader m) => Ident -> Span -> m StmtRead
mkStmtRead ident span = do
  dataType <- identDataType ident
  unless (dataType == Symbol.DataTypeInt) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeInt]
    dataType
    span
  return $ MkStmtRead ident span

data StmtWrite = MkStmtWrite Exp Span

mkStmtWrite
  :: (MonadError Error m, SymbolTableReader m) => Exp -> Span -> m StmtWrite
mkStmtWrite exp span = do
  dataType <- expDataType exp
  unless (dataType == Symbol.DataTypeInt) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeInt]
    dataType
    span
  return $ MkStmtWrite exp span

data StmtIf = MkStmtIf Exp [Stmt] Span

mkStmtIf
  :: (MonadError Error m, SymbolTableReader m)
  => Exp
  -> [Stmt]
  -> Span
  -> m StmtIf
mkStmtIf exp body span = do
  dataType <- expDataType exp
  unless (dataType == Symbol.DataTypeBool) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeBool]
    dataType
    span
  return $ MkStmtIf exp body span

data StmtIfElse = MkStmtIfElse Exp [Stmt] [Stmt] Span

mkStmtIfElse
  :: (MonadError Error m, SymbolTableReader m)
  => Exp
  -> [Stmt]
  -> [Stmt]
  -> Span
  -> m StmtIfElse
mkStmtIfElse exp thenBody elseBody span = do
  dataType <- expDataType exp
  unless (dataType == Symbol.DataTypeBool) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeBool]
    dataType
    span
  return $ MkStmtIfElse exp thenBody elseBody span

pushLoopStack :: (LoopStackReader m, LoopStackWriter m) => m ()
pushLoopStack = getLoopStack >>= (putLoopStack . LoopStack.push)

popLoopStack :: (LoopStackReader m, LoopStackWriter m) => m ()
popLoopStack = getLoopStack >>= (putLoopStack . fromJust . LoopStack.pop)


data StmtWhile = MkStmtWhile Exp [Stmt] Span

mkStmtWhile
  :: (MonadError Error m, SymbolTableReader m)
  => Exp
  -> [Stmt]
  -> Span
  -> m StmtWhile
mkStmtWhile exp body span = do
  dataType <- expDataType exp
  unless (dataType == Symbol.DataTypeBool) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeBool]
    dataType
    span
  return $ MkStmtWhile exp body span

data StmtDoWhile = MkStmtDoWhile Exp [Stmt] Span

mkStmtDoWhile
  :: (MonadError Error m, SymbolTableReader m)
  => Exp
  -> [Stmt]
  -> Span
  -> m StmtDoWhile
mkStmtDoWhile exp body span = do
  dataType <- expDataType exp
  unless (dataType == Symbol.DataTypeBool) $ throwError $ Error.typeNotAllowed
    [Symbol.DataTypeBool]
    dataType
    span
  return $ MkStmtDoWhile exp body span

data StmtBreak = MkStmtBreak Span

mkStmtBreak :: (MonadError Error m, LoopStackReader m) => Span -> m StmtBreak
mkStmtBreak span = do
  loopStack <- getLoopStack
  _         <- LoopStack.pop loopStack |> throwOutOfLoop
  return $ MkStmtBreak span
  where throwOutOfLoop = liftEither . maybeToEither (Error.syntaxError span)

data StmtContinue = MkStmtContinue Span

mkStmtContinue
  :: (MonadError Error m, LoopStackReader m) => Span -> m StmtContinue
mkStmtContinue span = do
  loopStack <- getLoopStack
  _         <- LoopStack.pop loopStack |> throwOutOfLoop
  return $ MkStmtContinue span
  where throwOutOfLoop = liftEither . maybeToEither (Error.syntaxError span)

data OpArithmetic = OpAdd | OpSub | OpMul | OpDiv | OpMod

data OpLogical = OpLT | OpGT | OpLE | OpGE | OpEQ | OpNE

data Exp = ExpIdent ExpIdent | ExpPure ExpPure

data ExpIdent = MkExpIdent Ident

data ExpPure
  = ExpNum Int Span
  | ExpArithmetic Exp OpArithmetic Exp Span
  | ExpLogical Exp OpLogical Exp Span


-- Helper Functions

identSymbol :: (SymbolTableReader m) => Ident -> m Symbol
identSymbol ident = do
  let (MkIdent identName _) = ident
  fromJust . SymbolTable.lookup identName <$> getSymtab

identDataType
  :: (MonadError Error m, SymbolTableReader m) => Ident -> m DataType
identDataType ident = Symbol.dataType <$> identSymbol ident

expDataType :: (MonadError Error m, SymbolTableReader m) => Exp -> m DataType
expDataType (ExpIdent exp) = expIdentDataType exp
expDataType (ExpPure  exp) = return $ expPureDataType exp

expPureDataType :: ExpPure -> DataType
expPureDataType exp = case exp of
  ExpNum{}        -> DataTypeInt
  ExpArithmetic{} -> DataTypeInt
  ExpLogical{}    -> DataTypeBool

expIdentDataType
  :: (MonadError Error m, SymbolTableReader m) => ExpIdent -> m DataType
expIdentDataType exp = identDataType ident where MkExpIdent ident = exp

-- HasSpan instances

instance HasSpan Ident where
  getSpan (MkIdent _ span) = span

instance HasSpan Exp where
  getSpan exp = case exp of
    ExpIdent exp -> getSpan exp
    ExpPure  exp -> getSpan exp

instance HasSpan ExpIdent where
  getSpan exp = case exp of
    MkExpIdent ident -> getSpan ident

instance HasSpan ExpPure where
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
