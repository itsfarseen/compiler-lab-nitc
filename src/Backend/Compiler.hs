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

getIdentLocInStack :: String -> Compiler Int
getIdentLocInStack ident = (4096 +)
  <$> gets (symbolLoc . fromJust . SymbolTable.lookup ident . symbolTable)

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
  let loadLoc = 2048
  symbolTableLastLoc <- gets symbolTableLastLoc
  let setupCode = [XSM_MOV_Int SP 4096, XSM_ADD_I SP symbolTableLastLoc]
  let codeStart = loadLoc + length header + ((length setupCode) * 2)
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

getTranslatedCodeNumbered :: Compiler String
getTranslatedCodeNumbered = do
  let header = zipWith (\i s -> show i ++ ":\t" ++ s)
                       [2048 :: Int ..]
                       ["0", "2056", "0", "0", "0", "0", "0", "0"]
  let loadLoc = 2048
  symbolTableLastLoc <- gets symbolTableLastLoc
  let setupCode = [XSM_MOV_Int SP 4096, XSM_ADD_I SP symbolTableLastLoc]
  let codeStart = loadLoc + length header + ((length setupCode) * 2)
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
  let body = map toCode instrsTranslated
  let codeLines =
        header
          ++ zipWith (\i s -> show i ++ ":\t" ++ s) [2056 :: Int, 2058 ..] body

  return $ unlines codeLines

getUntranslatedCode :: Compiler String
getUntranslatedCode = do
  let header = ["0", "2056", "0", "0", "0", "0", "0", "0"]
  let loadLoc   = 2048
  let codeStart = length header + loadLoc
  symbolTableLastLoc <- gets symbolTableLastLoc
  let setupCode = [XSM_MOV_Int SP 4096, XSM_ADD_I SP symbolTableLastLoc]
  compiler <- get
  let codeLabelled = prependLabels (map toCodeUntranslated $ code compiler)
                                   0
                                   (HM.toList $ labels compiler)
  let codeLines = header ++ map toCodeUntranslated setupCode ++ codeLabelled
  return $ intercalate "\n" codeLines

prependLabels :: [String] -> Int -> [(String, Int)] -> [String]
prependLabels code i labels = case labels of
  []                     -> code
  ((label, j) : labels') -> case code of
    []          -> (label ++ ":\t") : prependLabels [] (i + 1) labels'
    (c : code') -> if i == j
      then
        let c' = label ++ ":\t" ++ c
        in  c' : prependLabels code' (i + 1) labels'
      else ("\t" ++ c) : prependLabels code' (i + 1) labels


labelTranslate :: Label -> Compiler Int
labelTranslate label = do
  compiler <- get
  return $ labels compiler HM.! label
