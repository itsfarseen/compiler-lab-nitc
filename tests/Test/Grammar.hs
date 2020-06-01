module Test.Grammar where

import Grammar
import Error


data TestMState = TestMState {
    tsSymbols :: [Symbol]
}

data TestM = StateT TestMState (Except Error)

