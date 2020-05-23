module Backend.CompilerUtils where

import           Control.Monad.Except (throwError)
import           Control.Monad.State.Strict
import qualified Data.HashMap.Strict as HM
import           Data.List (intercalate)
import           Data.Maybe (fromJust)

import           Backend.Compiler
import           Backend.Instructions
import           Backend.Reg
import           Error
import           Span
import           SymbolTable (SymbolTable)
import qualified SymbolTable
import Grammar
import Symbol

getIdentLocInStack :: Ident -> Compiler Int
getIdentLocInStack ident = (4096 +)
  <$> gets (symbolLoc . fromJust . SymbolTable.lookup identName . symbolTable)
  where MkIdent identName _ = ident

getIdentDataType :: Ident -> Compiler DataType
getIdentDataType ident = gets
  (Symbol.dataType . fromJust . SymbolTable.lookup identName . symbolTable)
  where MkIdent identName _ = ident

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
