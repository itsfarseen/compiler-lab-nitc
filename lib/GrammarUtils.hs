module GrammarUtils where

import Grammar 

data GState =
  GState
    { gsSyms :: SymTabGlobal
    , gsFuncs :: [Func]
    , gsUserTypes :: [UserType]
    }

data LState =
  LState { lsSyms :: SymTabLocal
         , lsLoopStack :: LoopStack
         , lsIsTopLevel :: Bool
         , lsFuncDecl :: FuncDecl
         }

data TState = TState { tSyms :: SymTabLocal }

type TopLevelStmt = GState -> GState

gStateInit :: GState
gStateInit = GState (SymTabGlobal []) [] [] 

