{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Backend.Codegen where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.State.Strict
import Data.Either.Extra

import Grammar
import Span
import Backend.Codegen as Codegen
import qualified Backend.Simulator as Simulator
import Test.GrammarUtils
import Test.Utils
import Backend.Instructions (XSMInstr)

span0 :: Span
span0 = Span 0 0

cRun :: GrammarM a -> (a -> Codegen b) -> b
cRun g compfn =
  let (a, gstate) = fromRight' $ gRun g
      symbols     = head $ gsSymbols gstate
  in  fromRight' $ runCodegen (compfn a) symbols

execStmtsGetCode :: Foldable t => t Stmt -> Codegen [XSMInstr]
execStmtsGetCode stmts = do
  execSetupGlobalSymtab
  mapM_ execStmt stmts
  getCodeTranslated

execStmtGetCode :: Stmt -> Codegen [XSMInstr]
execStmtGetCode stmt = execStmtsGetCode [stmt]

test_execStmtAssign :: TestTree
test_execStmtAssign = testCaseSteps "execStmtAssign" $ \step -> do
  step "Simple assign"
  let
    code = cRun
      do
        doVarDeclare "foo" TypeInt [] span0
        lValue <- mkLValue (spanW "foo") []
        StmtAssign <$> mkStmtAssign lValue (RExp $ ExpNum 10) span0
      execStmtGetCode

  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"

  step "Assign self"
  let
    code = cRun
      do
        doVarDeclare "foo" TypeInt [] span0
        lValue <- mkLValue (spanW "foo") []
        s1     <-
          StmtAssign <$> mkStmtAssign lValue (RExp $ ExpNum 10) span0
        s2 <-
          StmtAssign <$> mkStmtAssign lValue (RLValue lValue) span0
        return [s1, s2]
      execStmtsGetCode

  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"


test_execStmtRead :: TestTree
test_execStmtRead = testCaseSteps "execStmtRead" $ \step -> do
  step "Single read"
  let code = cRun
        do
          doVarDeclare "foo" TypeInt [] span0
          lValue <- mkLValue (spanW "foo") []
          StmtRead <$> mkStmtRead (spanW lValue)
        execStmtGetCode

  let simulator = Simulator.run (Simulator.initWithStdin ["10"] code)
  Simulator.getMemory 4096 simulator @?= "10"

  step "Double read"
  let code = cRun
        do
          doVarDeclare "foo" TypeInt [] span0
          lValue <- mkLValue (spanW "foo") []
          s1     <- StmtRead <$> mkStmtRead (spanW lValue)
          s2     <- StmtRead <$> mkStmtRead (spanW lValue)
          return [s1, s2]
        execStmtsGetCode

  let simulator =
        Simulator.run (Simulator.initWithStdin ["10", "555"] code)
  Simulator.getMemory 4096 simulator @?= "555"

test_execStmtWrite :: TestTree
test_execStmtWrite = testCaseSteps "execStmtWrite" $ \step -> do
  step "Simple write"
  let code = cRun
        do
          s1 <- StmtWrite <$> mkStmtWrite (spanW (RExp $ ExpNum 100))
          s2 <- StmtWrite
            <$> mkStmtWrite (spanW (RExp $ ExpStr "ASD"))
          return [s1, s2]
        execStmtsGetCode

  let simulator =
        Simulator.run (Simulator.initWithStdin ["10", "555"] code)
  Simulator.getStdout simulator @?= ["100", "ASD"]

test_execStmtIf :: TestTree
test_execStmtIf = testCaseSteps "execStmtIf" $ \step -> do
  step "If stmt true"
  let
    code = cRun
      do
        cond <- mkExpLogical (spanW $ RExp $ ExpNum 1)
                             OpEQ
                             (spanW $ RExp $ ExpNum 1)
        s1 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 100)
        s2 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 200)
        s3 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 10)
        s  <- StmtIf <$> mkStmtIf (spanW $ RExp cond) [s1, s2, s3]
        s4 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 555)
        s5 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 666)
        return [s, s4, s5]
      execStmtsGetCode

  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["100", "200", "10", "555", "666"]

  step "If stmt false"
  let
    code = cRun
      do
        cond <- mkExpLogical (spanW $ RExp $ ExpNum 1)
                             OpNE
                             (spanW $ RExp $ ExpNum 1)
        s1 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 100)
        s2 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 200)
        s3 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 10)
        s  <- StmtIf <$> mkStmtIf (spanW $ RExp cond) [s1, s2, s3]
        s4 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 123)
        s5 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 456)
        return [s, s4, s5]
      execStmtsGetCode

  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["123", "456"]

test_execStmtIfElse :: TestTree
test_execStmtIfElse = testCaseSteps "execStmtIfElse" $ \step -> do
  step "IfElse stmt true"
  let
    code = cRun
      do
        cond <- mkExpLogical (spanW $ RExp $ ExpNum 1)
                             OpEQ
                             (spanW $ RExp $ ExpNum 1)
        then1 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 100)
        then2 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 200)
        then3 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 10)
        else1 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 110)
        else2 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 210)
        s     <- StmtIfElse <$> mkStmtIfElse (spanW $ RExp cond)
                                             [then1, then2, then3]
                                             [else1, else2]
        s1 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 123)
        s2 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 456)
        return [s, s1, s2]
      execStmtsGetCode

  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["100", "200", "10", "123", "456"]

  step "IfElse stmt false"
  let
    code = cRun
      do
        cond <- mkExpLogical (spanW $ RExp $ ExpNum 1)
                             OpNE
                             (spanW $ RExp $ ExpNum 1)
        then1 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 100)
        then2 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 200)
        then3 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 10)
        else1 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 110)
        else2 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 210)
        s     <- StmtIfElse <$> mkStmtIfElse (spanW $ RExp cond)
                                             [then1, then2, then3]
                                             [else1, else2]
        s1 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 123)
        s2 <- StmtWrite <$> mkStmtWrite (spanW $ RExp $ ExpNum 456)
        return [s, s1, s2]
      execStmtsGetCode

  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["110", "210", "123", "456"]

test_execStmtWhile :: TestTree
test_execStmtWhile = testCaseSteps "execStmtWhile" $ \step -> do
  step "While stmt true"
  let
    code = cRun
      do
        doVarDeclare "foo" TypeInt [] span0
        lValue <- mkLValue (spanW "foo") []
        s0     <-
          StmtAssign <$> mkStmtAssign lValue (RExp $ ExpNum 0) span0
        cond <- mkExpLogical (spanW $ RLValue lValue)
                             OpLT
                             (spanW $ RExp $ ExpNum 10)
        rhs <- mkExpArithmetic (spanW $ RLValue lValue)
                               OpAdd
                               (spanW $ RExp $ ExpNum 1)
        s1  <- StmtAssign <$> mkStmtAssign lValue (RExp $ rhs) span0
        s2  <- StmtWhile <$> mkStmtWhile (spanW $ RExp cond) [s1]
        rhs <- mkExpArithmetic (spanW $ RLValue lValue)
                               OpMul
                               (spanW $ RExp $ ExpNum 2)

        s3 <- StmtAssign <$> mkStmtAssign lValue (RExp $ rhs) span0
        return [s0, s2, s3]
      execStmtsGetCode

  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "20"

  step "While stmt false"
  let
    code = cRun
      do
        doVarDeclare "foo" TypeInt [] span0
        lValue <- mkLValue (spanW "foo") []
        s0     <-
          StmtAssign <$> mkStmtAssign lValue (RExp $ ExpNum 15) span0
        cond <- mkExpLogical (spanW $ RLValue lValue)
                             OpGT
                             (spanW $ RExp $ ExpNum 20)
        rhs <- mkExpArithmetic (spanW $ RLValue lValue)
                               OpAdd
                               (spanW $ RExp $ ExpNum 1)

        s1  <- StmtAssign <$> mkStmtAssign lValue (RExp $ rhs) span0
        s2  <- StmtWhile <$> mkStmtWhile (spanW $ RExp cond) [s1]
        rhs <- mkExpArithmetic (spanW $ RLValue lValue)
                               OpMul
                               (spanW $ RExp $ ExpNum 2)
        s3 <- StmtAssign <$> mkStmtAssign lValue (RExp $ rhs) span0
        return [s0, s2, s3]
      execStmtsGetCode
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "30"

test_execStmtBreak :: TestTree
test_execStmtBreak = testCaseSteps "execStmtBreak" $ \_ -> do
  let
    code = cRun
      do
        doVarDeclare "foo" TypeInt [] span0
        lValue <- mkLValue (spanW "foo") []
        s0     <-
          StmtAssign <$> mkStmtAssign lValue (RExp $ ExpNum 15) span0
        cond <- mkExpLogical (spanW $ RLValue lValue)
                             OpLT
                             (spanW $ RExp $ ExpNum 20)
        rhs <- mkExpArithmetic (spanW $ RLValue lValue)
                               OpAdd
                               (spanW $ RExp $ ExpNum 1)

        s1 <- StmtAssign <$> mkStmtAssign lValue (RExp $ rhs) span0
        pushLoop
        sb  <- StmtBreak <$> mkStmtBreak span0
        s2  <- StmtWhile <$> mkStmtWhile (spanW $ RExp cond) [s1, sb]
        rhs <- mkExpArithmetic (spanW $ RLValue lValue)
                               OpMul
                               (spanW $ RExp $ ExpNum 2)

        s3 <- StmtAssign <$> mkStmtAssign lValue (RExp $ rhs) span0
        return [s0, s2, s3]
      execStmtsGetCode
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "32"


test_execStmtContinue :: TestTree
test_execStmtContinue = testCaseSteps "execStmtContinue" $ \_ -> do
  let
    code = cRun
      do
        doVarDeclare "foo" TypeInt [] span0
        lValue <- mkLValue (spanW "foo") []
        s0     <-
          StmtAssign <$> mkStmtAssign lValue (RExp $ ExpNum 15) span0
        cond <- mkExpLogical (spanW $ RLValue lValue)
                             OpLT
                             (spanW $ RExp $ ExpNum 20)
        rhs <- mkExpArithmetic (spanW $ RLValue lValue)
                               OpAdd
                               (spanW $ RExp $ ExpNum 1)

        s1 <- StmtAssign <$> mkStmtAssign lValue (RExp $ rhs) span0
        pushLoop
        sc  <- StmtContinue <$> mkStmtContinue span0
        rhs <- mkExpArithmetic (spanW $ RLValue lValue)
                               OpMul
                               (spanW $ RExp $ ExpNum 100)

        sd <- StmtAssign <$> mkStmtAssign lValue (RExp $ rhs) span0
        s2 <- StmtWhile
          <$> mkStmtWhile (spanW $ RExp cond) [s1, sc, sd]
        rhs <- mkExpArithmetic (spanW $ RLValue lValue)
                               OpMul
                               (spanW $ RExp $ ExpNum 2)

        s3 <- StmtAssign <$> mkStmtAssign lValue (RExp $ rhs) span0
        return [s0, s2, s3]
      execStmtsGetCode
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "40"


----------------------------------------------------------------------------------------

test_getLValueLocInReg :: TestTree
test_getLValueLocInReg =
  testCaseSteps "getLValueLocInReg" $ \step -> do
    step "Simple"

    let (code :: [XSMInstr], r1, r2) = cRun
          do
            doVarDeclare "foo" TypeInt [] span0
            doVarDeclare "bar" TypeInt [] span0
            lValue1 <- mkLValue (spanW "foo") []
            lValue2 <- mkLValue (spanW "bar") []
            return (lValue1, lValue2)
          (\(lValue1, lValue2) -> do
            r1   <- getLValueLocInReg (lValue1)
            r2   <- getLValueLocInReg (lValue2)
            code <- gets Codegen.code
            return (code, r1, r2)
          )

    let simulator = Simulator.run (Simulator.init code)
    let loc1      = Simulator.getRegVal r1 simulator
    let loc2      = Simulator.getRegVal r2 simulator
    (read loc1 >= (4096 :: Int)) @? "Variables must be in stack"
    (read loc2 >= (4096 :: Int)) @? "Variables must be in stack"
    loc1 /= loc2 @? "Two variables have different location"


    step "2D Array index"
    let
      (code, basereg, reg) = cRun
        do
          doVarDeclare "foo" TypeInt [3, 2] span0
          doVarDeclare "bar" TypeInt [4, 5] span0
          l1 <- mkLValue (spanW "bar") []
          l2 <- mkLValue (spanW "bar")
                         (spanW . RExp . ExpNum <$> [2, 3])
          return (l1, l2)
        (\(l1, l2) -> do
          base <- getLValueLocInReg l1
          r    <- getLValueLocInReg l2
          code <- gets Codegen.code
          return (code, base, r)
        )
    let simulator = Simulator.run (Simulator.init code)
    let loc       = read (Simulator.getRegVal reg simulator) :: Int
    let base = read (Simulator.getRegVal basereg simulator) :: Int
    loc @?= base + 2 * 5 + 3

