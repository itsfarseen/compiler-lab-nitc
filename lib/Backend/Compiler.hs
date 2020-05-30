{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module Backend.Compiler where

import Backend.Instructions
import Backend.Reg
import Control.Monad.Except
import Control.Monad.State
import qualified Data.HashMap.Strict as HM
import qualified Grammar
import Span
import Error (Error)
import qualified Error
import Backend.Codegen
import Data.List
import Data.Maybe

type Compiler = StateT CompilerState (Either Error)

data CompilerState =
  CompilerState
    { freeRegs :: [Reg]
    , code :: [XSMInstr]
    , labels :: HM.HashMap String Int
    , lastLabelNo :: Int
    , loopBreakLabels :: [String]
    , loopContinueLabels :: [String]
    , symbolTable :: [Symbol]
    , symbolTableLastLoc :: Int
    }

initCompilerState symbols = CompilerState { freeRegs           = [R0 .. R19]
                                          , code               = []
                                          , labels             = HM.empty
                                          , lastLabelNo        = 0
                                          , loopBreakLabels    = []
                                          , loopContinueLabels = []
                                          , symbolTable
                                          , symbolTableLastLoc
                                          }
  where (symbolTable, symbolTableLastLoc) = buildSymbolTable symbols 0 0

data Symbol = Symbol {
    symName :: String,
    symDataType :: Grammar.DataType,
    symRelLoc :: Int
  }

buildSymbolTable symbols locLast locNext = case symbols of
  [] -> ([], locLast)
  (sym : syms) ->
    let Grammar.Symbol { symName, symDataType } = sym
        Grammar.DataType dims _ = symDataType
        size                    = product dims
        loc                     = locNext
        (syms', locLast)        = buildSymbolTable syms loc (loc + size)
    in  ((Symbol { symName, symDataType, symRelLoc = loc } : syms'), locLast)


runCompiler :: Compiler a -> [Grammar.Symbol] -> Either Error a
runCompiler compiler symbols = evalStateT compiler (initCompilerState symbols)

instance Idents Compiler where
  getIdentLocInStack ident = (4096 +) <$> gets
    (symRelLoc . fromJust . find (\s -> symName s == identName) . symbolTable)
    where Grammar.MkIdent identName = ident

  getIdentDataType ident = gets
    (symDataType . fromJust . find (\s -> symName s == identName) . symbolTable)
    where Grammar.MkIdent identName = ident

instance FreeRegs Compiler where
  getFreeReg = do
    compiler <- get
    case freeRegs compiler of
      (r : rs) -> do
        put $ compiler { freeRegs = rs }
        return r
      [] -> throwError $ Error.compilerError "out of registers" (Span 0 0)

  releaseReg reg = do
    compiler <- get
    put compiler { freeRegs = pushFreeReg (freeRegs compiler) reg }
    where pushFreeReg regs r = r : regs

instance Code Compiler where
  appendCode getInstrs' = do
    compiler <- get
    put compiler { code = code compiler ++ getInstrs' }

instance Labels Compiler where
  getNewLabel = do
    compiler <- get
    let newLabelNo = lastLabelNo compiler + 1
    put compiler { lastLabelNo = newLabelNo }
    return $ "L" ++ show newLabelNo

  installLabel label = do
    compiler <- get
    let nextLineNo = length (code compiler)
    put compiler { labels = HM.insert label nextLineNo (labels compiler) }

  pushLoopBreakLabel label = do
    compiler <- get
    put compiler { loopBreakLabels = loopBreakLabels compiler ++ [label] }

  peekLoopBreakLabel = gets (last . loopBreakLabels)

  popLoopBreakLabel  = do
    compiler <- get
    put compiler { loopBreakLabels = init (loopBreakLabels compiler) }

  pushLoopContinueLabel label = do
    compiler <- get
    put compiler { loopContinueLabels = loopContinueLabels compiler ++ [label] }

  peekLoopContinueLabel = gets (last . loopContinueLabels)

  popLoopContinueLabel  = do
    compiler <- get
    put compiler { loopContinueLabels = init (loopContinueLabels compiler) }
