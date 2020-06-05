{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Test.GrammarUtils where

import Control.Monad.Except (MonadError)
import Control.Monad.State.Strict
import Data.List (find)
import Error
import Grammar
import Test.Utils

data GrammarState = GrammarState
  { gsSymbols :: [[Symbol]],
    gsFuncs :: [Func],
    gsLoopStack :: Int
  }

initGrammarState :: GrammarState
initGrammarState = GrammarState [[]] [] 0

newtype GrammarM a
  = GrammarM (StateT GrammarState (Either Error) a)
  deriving
    ( Functor,
      Applicative,
      Monad,
      MonadError Error,
      MonadState GrammarState
    )

instance ReadSymbols GrammarM where
  symLookup name = gets
    (find (\x -> symName x == name) . concat . gsSymbols)

instance WriteSymbols GrammarM where
  symInsert sym = modify
    (\state ->
      let (symtab : ss) = gsSymbols state
          symtab'       = symtab ++ [sym]
          gsSymbols'    = (symtab' : ss)
      in  state { gsSymbols = gsSymbols' }
        -- Note: multiple insert will cause duplicates
    )

instance ReadFuncs GrammarM where
  funcLookup name = gets
    (find (\x -> (funcName . funcDecl) x == name) . gsFuncs)

instance WriteFuncs GrammarM where
  funcInsert func = modify
    (\state -> state
      { gsFuncs = insertList (funcName . funcDecl)
                             func
                             (gsFuncs state)
      }
    )

instance ReadLoopStack GrammarM where
  hasLoop = gets (\x -> gsLoopStack x > 0)

instance LoopStackWriter GrammarM where
  pushLoop =
    modify (\x -> x { gsLoopStack = gsLoopStack x + 1 })
  popLoop =
    modify
      (\x -> x { gsLoopStack = max 0 (gsLoopStack x - 1) })

instance SymbolTableStack GrammarM where
  symStackPush =
    modify (\x -> x { gsSymbols = [] : (gsSymbols x) })
  symStackPop = do
    (symtab, ss) <- gets gsSymbols >>= \case
      [] -> error "gsSymbols can't be empty"
      [_] -> error "can't pop symStack, only one symtab"
      (symtab : ss) -> return (symtab, ss)
    modify (\x -> x { gsSymbols = ss })
    return symtab

runGrammarM
  :: GrammarState -> GrammarM a -> Either Error (a, GrammarState)
runGrammarM state testM =
  let (GrammarM stateT) = testM in runStateT stateT state

