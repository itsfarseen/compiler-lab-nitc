module LoopStack ( LoopStack, init, push, pop) where

import Prelude hiding (init)

newtype LoopStack = LoopStack Int

init :: LoopStack
init = LoopStack 0

push :: LoopStack -> LoopStack
push (LoopStack i) = LoopStack (i + 1)

pop :: LoopStack -> Maybe LoopStack
pop (LoopStack i) | i < 1     = Nothing
                  | otherwise = Just (LoopStack $ i - 1)
