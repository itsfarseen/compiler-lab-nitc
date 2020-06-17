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
import Data.Maybe (isJust, fromJust)
import Data.Bifunctor (second)

data AlexInput = AlexInput {alexInputStr :: [Word8], alexTokenOffset :: Int }

data FrontendData =
  FrontendData
    { alexInput :: AlexInput
    , gSymbols :: [Symbol]
    , funcs :: [Func]
    , funcContext :: Maybe (FuncDecl, [Symbol])
    , loopStack :: Int
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
                   , gSymbols   = []
                   , funcs     = []
                   , funcContext = Nothing
                   , loopStack = 0
                   }

runFrontend :: FrontendData -> Frontend a -> Either Error a
runFrontend initData (Frontend state) = runExcept $ evalStateT state initData

-- Utilities

insertList :: Eq k => (a -> k) -> a -> [a] -> [a]
insertList key a list = case list of
  [] -> [a]
  (a' : as') ->
    (if key a == key a' then a : as' else a' : insertList key a as')

-- Instances

instance ReadSymbols Frontend where
  gSymLookup name = gets $ (find $ \s -> symName s == name) . gSymbols

instance WriteSymbols Frontend where
  gSymInsert symbol = modify $ \frontendData -> frontendData
    { gSymbols = insertList symName symbol (gSymbols frontendData)
    }

instance ReadFuncs Frontend where
  funcLookup name =
    gets $ (find $ \s -> (funcName . funcDecl $ s) == name) . funcs

instance WriteFuncs Frontend where
  funcInsert func = modify $ \frontendData -> frontendData
    { funcs = insertList (funcName . funcDecl) func (funcs frontendData)
    }

instance ReadLoopStack Frontend where
  hasLoop = gets (\x -> loopStack x > 0)

instance LoopStackWriter Frontend where
  pushLoop = modify (\x -> x{loopStack = loopStack x + 1})
  popLoop = modify (\x -> x{loopStack = max 0 (loopStack x - 1)})


instance FunctionContext Frontend where
  fcEnter funcDecl = modify $ \s ->
    s{funcContext = Just (funcDecl, [])}
  fcExit = do
    (_, syms) <- gets (fromJust . funcContext)
    modify $ \s -> s{funcContext = Nothing}
    return syms
  fcGet = gets (fst. fromJust . funcContext)
  fcSymLookup name = gets $ (find $ \s -> symName s == name) . snd . fromJust .funcContext
  fcSymInsert symbol = modify $ \frontendData -> frontendData
    { funcContext = fmap (second $ insertList symName symbol) (funcContext frontendData) }
  fcHasCtxt = gets (isJust . funcContext)

