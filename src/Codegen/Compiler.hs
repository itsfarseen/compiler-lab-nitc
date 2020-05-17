module Codegen.Compiler where

import Codegen.Instructions
import Codegen.Reg
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
                                         , symbolTableLastLoc = lastLoc
                                         }
 where
  (lastLoc, symTabExt) = SymbolTable.map genExt 0 symTab
  genExt lastLoc _ = (lastLoc + 1, lastLoc + 1)

runCompiler :: Compiler a -> SymbolTable () -> Either Error a
runCompiler compiler symtab = evalStateT compiler (initCompilerState symtab)


getIdentLocInStack :: String -> Compiler Int
getIdentLocInStack ident =
  gets (symbolLoc . fromJust . SymbolTable.lookup ident . symbolTable)

getFreeReg :: Compiler Reg
getFreeReg = do
  compiler <- get
  case freeRegs compiler of
    (r : rs) -> do
      put $ compiler { freeRegs = rs }
      return r
    [] -> throwError $ Error.compilerError "out of registers" (Span 0 0)

releaseReg :: Reg -> Compiler ()
releaseReg reg = do
  compiler <- get
  put compiler { freeRegs = pushFreeReg (freeRegs compiler) reg }
  where pushFreeReg regs r = r : regs

appendCode :: [XSMInstr] -> Compiler ()
appendCode getInstrs' = do
  compiler <- get
  put compiler { code = code compiler ++ getInstrs' }

getNewLabel :: Compiler String
getNewLabel = do
  compiler <- get
  let newLabelNo = lastLabelNo compiler + 1
  put compiler { lastLabelNo = newLabelNo }
  return $ "L" ++ show newLabelNo

installLabel :: String -> Compiler ()
installLabel label = do
  compiler <- get
  let nextLineNo = length (code compiler)
  put compiler { labels = HM.insert label nextLineNo (labels compiler) }

pushLoopBreakLabel :: String -> Compiler ()
pushLoopBreakLabel label = do
  compiler <- get
  put compiler { loopBreakLabels = loopBreakLabels compiler ++ [label] }

peekLoopBreakLabel :: Compiler String
peekLoopBreakLabel = gets (last . loopBreakLabels)

popLoopBreakLabel :: Compiler String
popLoopBreakLabel = do
  compiler <- get
  let label = last (loopBreakLabels compiler)
  put compiler { loopBreakLabels = init (loopBreakLabels compiler) }
  return label

pushLoopContinueLabel :: String -> Compiler ()
pushLoopContinueLabel label = do
  compiler <- get
  put compiler { loopContinueLabels = loopContinueLabels compiler ++ [label] }

peekLoopContinueLabel :: Compiler String
peekLoopContinueLabel = gets (last . loopContinueLabels)

popLoopContinueLabel :: Compiler String
popLoopContinueLabel = do
  compiler <- get
  let label = last (loopContinueLabels compiler)
  put compiler { loopContinueLabels = init (loopContinueLabels compiler) }
  return label

getTranslatedCode :: Compiler String
getTranslatedCode = do
  let header = ["0", "2056", "0", "0", "0", "0", "0", "0"]
  let loadLoc   = 2048
  let codeStart = length header + loadLoc
  symbolTableLastLoc <- (+ 4096) <$> gets symbolTableLastLoc
  let setupCode = [XSM_MOV_Int SP 4096, XSM_ADD_I SP symbolTableLastLoc]
  compiler <- get
  let instrs = setupCode ++ code compiler
  instrsTranslated <- mapM
    (\instr -> case instr of
      XSM_UTJ jmp -> do
        let label = utjGetLabel jmp
        loc <- labelTranslate label
        let loc' = (loc * 2) + codeStart
        return $ utjTranslate jmp loc'
      _ -> return instr
    )
    instrs
  let codeLines = header ++ map toCode instrsTranslated
  return $ intercalate "\n" codeLines

labelTranslate :: Label -> Compiler Int
labelTranslate label = do
  compiler <- get
  return $ labels compiler HM.! label
