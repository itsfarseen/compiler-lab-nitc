{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleContexts #-}

module Frontend where

import Codec.Binary.UTF8.String as UString
import Control.Monad.State.Strict
import Control.Monad.Except
import Data.Word
import Error (Error)

data AlexInput = AlexInput {alexInputStr :: [Word8], alexTokenOffset :: Int }

data FrontendData =
  FrontendData
    { alexInput :: AlexInput
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
  let
    alexInput = AlexInput
      { alexInputStr    = UString.encode sourceCode
      , alexTokenOffset = 0
      }
  in FrontendData { alexInput }

runFrontend :: FrontendData -> Frontend a -> Either Error a
runFrontend initData (Frontend state) =
  runExcept $ evalStateT state initData

throwError :: Error -> Frontend a
throwError e = 
  liftEither $ Left e
