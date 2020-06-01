{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

import Codec.Binary.UTF8.String as UString
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Word
import Data.List
import Error (Error)
import Grammar
import LoopStack

data AlexInput = AlexInput {alexInputStr :: [Word8], alexTokenOffset :: Int }

data FrontendData =
  FrontendData
    { alexInput :: AlexInput
    , symbols :: [Symbol]
    , funcs :: [Func]
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

initData :: String -> FrontendData
initData sourceCode =
  let alexInput = AlexInput { alexInputStr    = UString.encode sourceCode
                            , alexTokenOffset = 0
                            }
  in  FrontendData { alexInput
                   , symbols   = []
                   , funcs     = []
                   , loopStack = LoopStack.init
                   }

runFrontend :: FrontendData -> Frontend a -> Either Error a
runFrontend initData (Frontend state) = runExcept $ evalStateT state initData

-- Instances

insertList :: Eq k => (a -> k) -> a -> [a] -> [a]
insertList key a list = case list of
  [] -> [a]
  (a' : as') ->
    (if key a == key a' then a : as' else a' : insertList key a as')


instance ReadSymbols Frontend where
  symLookup name = gets $ (find $ \s -> symName s == name) . symbols

instance WriteSymbols Frontend where
  symInsert symbol = modify $ \frontendData -> frontendData
    { symbols = insertList symName symbol (symbols frontendData)
    }

instance ReadFuncs Frontend where
  funcLookup name =
    gets $ (find $ \s -> (funcName . funcDecl $ s) == name) . funcs

instance WriteFuncs Frontend where
  funcInsert func = modify $ \frontendData -> frontendData
    { funcs = insertList (funcName . funcDecl) func (funcs frontendData)
    }

instance LoopStackReader Frontend where
  getLoopStack = gets loopStack

instance LoopStackWriter Frontend where
  putLoopStack loopStack = modify $ \frontendData -> frontendData { loopStack }

instance SymbolTableStack Frontend where
