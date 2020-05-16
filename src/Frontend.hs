{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Frontend where

import Codec.Binary.UTF8.String as UString
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Word
import Error (Error)
import Grammar
import SymbolTable
import LoopStack

data AlexInput = AlexInput {alexInputStr :: [Word8], alexTokenOffset :: Int }

data FrontendData =
  FrontendData
    { alexInput :: AlexInput
    , symbolTable :: SymbolTable ()
    , loopStack :: LoopStack
    }

newtype Frontend a =
  Frontend (StateT FrontendData (Except Error) a)
  deriving (Functor, Applicative, Monad,MonadError Error, MonadState FrontendData)

class Monad m => AlexInputState m where
  getAlexInput :: m AlexInput
  putAlexInput :: AlexInput -> m ()

instance AlexInputState Frontend where
  getAlexInput = gets alexInput
  putAlexInput alexInput = modify $ \s -> s { alexInput }

-- init :: String -> Frontend
-- init sourceCode = do
--   PState { psInput = UString.encode sourceCode, psTokenOffset = 0 }

-- evalP :: P a -> PState -> Either E.Error a
-- evalP = evalStateT

-- Instances

instance SymbolTableReader Frontend where
  -- getSymtab :: m SymbolTable
  getSymtab = gets symbolTable

instance SymbolTableWriter Frontend where
  -- putSymtab :: SymbolTable -> m ()
  putSymtab symtab =
    modify $ \frontendData -> frontendData { symbolTable = symtab }

instance LoopStackReader Frontend where
  -- getLoopStack :: m LoopStack
  getLoopStack = gets loopStack
