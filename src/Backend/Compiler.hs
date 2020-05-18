module Backend.Compiler where

import Backend.Instructions
import Backend.Reg
import Control.Monad.Except
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Grammar
import qualified Symbol
import Symbol (Symbol)
import SymbolTable (SymbolTable)
import qualified SymbolTable
import Span
import Error (Error)
import qualified Error

type Compiler = StateT CompilerState (Either Error)

data CompilerState =
  CompilerState
    { freeRegs :: [Reg]
    , code :: [XSMInstr]
    , labels :: HM.HashMap String Int
    , lastLabelNo :: Int
    , loopBreakLabels :: [String]
    , loopContinueLabels :: [String]
    , symbolTable :: SymbolTableExt
    , symbolTableLastLoc :: Int
    }

type SymbolExtData = Int
type SymbolExt = Symbol SymbolExtData
type SymbolTableExt = SymbolTable SymbolExtData

symbolLoc :: SymbolExt -> Int
symbolLoc = Symbol.ext

initCompilerState symTab = CompilerState { freeRegs           = [R0 .. R19]
                                         , code               = []
                                         , labels             = HM.empty
                                         , lastLabelNo        = 0
                                         , loopBreakLabels    = []
                                         , loopContinueLabels = []
                                         , symbolTable        = symTabExt
                                         , symbolTableLastLoc = nextLoc - 1
                                         }
 where
  (nextLoc, symTabExt) = SymbolTable.map genExt 0 symTab
  genExt lastLoc _ = (lastLoc + 1, lastLoc)

runCompiler :: Compiler a -> SymbolTable () -> Either Error a
runCompiler compiler symtab = evalStateT compiler (initCompilerState symtab)
