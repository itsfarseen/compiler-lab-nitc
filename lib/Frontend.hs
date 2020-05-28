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
  in  FrontendData { alexInput, symbols = [], loopStack = LoopStack.init }

runFrontend :: FrontendData -> Frontend a -> Either Error a
runFrontend initData (Frontend state) = runExcept $ evalStateT state initData

-- Instances

instance ReadSymbols Frontend where
  symLookup name = gets $ (find $ \s -> symName s == name) . symbols

instance WriteSymbols Frontend where
  symInsert symbol = do
    let name = symName symbol
    maybeSymbol <- symLookup name
    case maybeSymbol of
      Just symbol -> return $ Left $ SymbolExists symbol
      Nothing     -> do
        modify $ \frontendData ->
          frontendData { symbols = (symbols frontendData) ++ [symbol] }
        return $ Right ()

instance LoopStackReader Frontend where
  -- getLoopStack :: m LoopStack
  getLoopStack = gets loopStack

instance LoopStackWriter Frontend where
  -- getLoopStack :: m LoopStack
  putLoopStack loopStack = modify $ \frontendData -> frontendData { loopStack }
