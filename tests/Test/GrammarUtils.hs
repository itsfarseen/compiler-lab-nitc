{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
module Test.GrammarUtils where

import Control.Monad.Except (MonadError)
import Control.Monad.State.Strict
import Data.List (find)
import Error
import Grammar
import Test.Utils
import Data.Maybe (fromJust, isJust)
import Data.Bifunctor (second)

data GrammarState =
  GrammarState
    { gsSymbols :: [[Symbol]]
    , gsFuncs :: [Func]
    , gsLoopStack :: Int
    , funcContext :: Maybe (FuncDecl, [Symbol])
    }

initGrammarState :: GrammarState
initGrammarState = GrammarState [[]] [] 0 Nothing

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
  symLookup name =
    gets (find (\x -> symName x == name) . concat . gsSymbols)

instance WriteSymbols GrammarM where
  gSymInsert sym = modify
    (\state ->
      let
        (symtab : ss) = gsSymbols state
        symtab'       = symtab ++ [sym]
        gsSymbols'    = (symtab' : ss)
      in state { gsSymbols = gsSymbols' }
        -- Note: multiple insert will cause duplicates
    )

instance ReadFuncs GrammarM where
  funcLookup name =
    gets (find (\x -> (funcName . funcDecl) x == name) . gsFuncs)

instance WriteFuncs GrammarM where
  funcInsert func = modify
    (\state -> state
      { gsFuncs = insertList
        (funcName . funcDecl)
        func
        (gsFuncs state)
      }
    )

instance ReadLoopStack GrammarM where
  hasLoop = gets (\x -> gsLoopStack x > 0)

instance LoopStackWriter GrammarM where
  pushLoop = modify (\x -> x { gsLoopStack = gsLoopStack x + 1 })
  popLoop =
    modify (\x -> x { gsLoopStack = max 0 (gsLoopStack x - 1) })

instance FunctionContext GrammarM where
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

runGrammarM
  :: GrammarState -> GrammarM a -> Either Error (a, GrammarState)
runGrammarM state testM =
  let (GrammarM stateT) = testM in runStateT stateT state

gRun :: GrammarM a -> Either Error (a, GrammarState)
gRun = runGrammarM initGrammarState

gAssertError :: GrammarM a -> IO ()
gAssertError = assertError . gRun

gAssertRight :: GrammarM a -> IO ()
gAssertRight x = assertRight $ gRun x >> return ()
