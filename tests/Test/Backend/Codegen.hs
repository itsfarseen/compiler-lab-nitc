{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Backend.Codegen where

import Control.Monad.State.Strict
import Data.HashMap.Strict as HM
import Data.Maybe
import Grammar
import Symbol

import Backend.Instructions
import Backend.CompilerUtils

newtype CompilerStub a =
  CompilerStub (State CompilerStubData a)
  deriving (Functor, Applicative, Monad, MonadState CompilerStubData)

data CompilerStubData =
  CompilerStubData
    { idents :: HM.HashMap String (Int, DataType)
    , code :: [XSMInstr]
    }


instance CompilerClass CompilerStub where
  appendCode code' = modify (\state -> state { code = code state ++ code' })
  getIdentLocInStack ident = gets
    (fst . fromJust . HM.lookup identName . idents)
    where MkIdent identName _ = ident
  getIdentDataType ident = gets (snd . fromJust . HM.lookup identName . idents)
    where MkIdent identName _ = ident
  getFreeReg = 



-- unit_getLValueLocInReg = do
--     let lValue 
