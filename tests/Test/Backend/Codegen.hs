{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Backend.Codegen where

import Test.Tasty
import Test.Tasty.HUnit
import Control.Monad.State.Strict
import Data.Either.Extra

import Grammar
import Span
import Backend.Codegen
import Backend.CodeUtils
import qualified Backend.Compiler as Compiler
import qualified Backend.Simulator as Simulator

spanNull :: Span
spanNull = Span 0 0

test_execStmtAssign :: TestTree
test_execStmtAssign = testCaseSteps "execStmtAssign" $ \step -> do
  let symbols =
        [ Symbol { symName     = "foo"
                 , symDataType = DataType [] TypeInt
                 , symDeclSpan = spanNull
                 }
        ]

  step "Simple assign"
  let code = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          execStmtAssign
            $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 10)
          code <- gets Compiler.code
          return code
        )
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"

  step "Assign self"
  let code = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          execStmtAssign
            $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 10)
          execStmtAssign $ MkStmtAssign (LValue [] "foo")
                                        (RLValue $ LValue [] "foo")
          code <- gets Compiler.code
          return code
        )
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"


test_execStmtRead :: TestTree
test_execStmtRead = testCaseSteps "execStmtRead" $ \step -> do
  let symbols =
        [ Symbol { symName     = "foo"
                 , symDataType = DataType [] TypeInt
                 , symDeclSpan = spanNull
                 }
        ]

  step "Single read"
  let code = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          execStmtRead (MkStmtRead (LValue [] "foo"))
          code <- gets Compiler.code
          return code
        )


  let simulator = Simulator.run (Simulator.initWithStdin ["10"] code)
  Simulator.getMemory 4096 simulator @?= "10"

  step "Double read"
  let code = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          execStmtRead (MkStmtRead (LValue [] "foo"))
          execStmtRead (MkStmtRead (LValue [] "foo"))
          code <- gets Compiler.code
          return code
        )
  let simulator =
        Simulator.run (Simulator.initWithStdin ["10", "555"] code)
  Simulator.getMemory 4096 simulator @?= "555"

test_execStmtWrite :: TestTree
test_execStmtWrite = testCaseSteps "execStmtWrite" $ \step -> do
  step "Simple write"
  let code = fromRight' $ flip
        Compiler.runCompiler
        []
        (do
          execStmtWrite (MkStmtWrite (RExp $ ExpNum 100))
          execStmtWrite (MkStmtWrite (RExp $ ExpStr "ASD"))
          code <- gets Compiler.code
          return code
        )
  let simulator =
        Simulator.run (Simulator.initWithStdin ["10", "555"] code)
  Simulator.getStdout simulator @?= ["100", "ASD"]

test_execStmtIf :: TestTree
test_execStmtIf = testCaseSteps "execStmtIf" $ \step -> do
  step "If stmt true"
  let
    code = fromRight' $ flip
      Compiler.runCompiler
      []
      (do
        execStmtIf $ MkStmtIf
          ( RExp
          $ MkExpLogical (RExp $ ExpNum 1) OpEQ (RExp $ ExpNum 1)
          )
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 100)
              , MkStmtWrite (RExp $ ExpNum 200)
              , MkStmtWrite (RExp $ ExpNum 10)
              ]
          )
        code <- getTranslatedInstrs

        return code
      )
  let simulator =
        Simulator.run (Simulator.initWithStdin ["10", "555"] code)
  Simulator.getStdout simulator @?= ["100", "200", "10"]

  step "If stmt false"
  let
    code = fromRight' $ flip
      Compiler.runCompiler
      []
      (do
        execStmtIf $ MkStmtIf
          ( RExp
          $ MkExpLogical (RExp $ ExpNum 1) OpNE (RExp $ ExpNum 1)
          )
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 100)
              , MkStmtWrite (RExp $ ExpNum 200)
              , MkStmtWrite (RExp $ ExpNum 10)
              ]
          )
        code <- getTranslatedInstrs
        return code
      )
  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= []



test_execStmtIfElse :: TestTree
test_execStmtIfElse = testCaseSteps "execStmtIfElse" $ \step -> do
  step "IfElse stmt true"
  let
    code = fromRight' $ flip
      Compiler.runCompiler
      []
      (do
        execStmtIfElse $ MkStmtIfElse
          ( RExp
          $ MkExpLogical (RExp $ ExpNum 1) OpEQ (RExp $ ExpNum 1)
          )
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 100)
              , MkStmtWrite (RExp $ ExpNum 200)
              , MkStmtWrite (RExp $ ExpNum 10)
              ]
          )
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 999)
              , MkStmtWrite (RExp $ ExpNum 888)
              , MkStmtWrite (RExp $ ExpNum 666)
              ]
          )
        code <- getTranslatedInstrs

        return code
      )
  let simulator =
        Simulator.run (Simulator.initWithStdin ["10", "555"] code)
  Simulator.getStdout simulator @?= ["100", "200", "10"]

  step "IfElse stmt false"
  let
    code = fromRight' $ flip
      Compiler.runCompiler
      []
      (do
        execStmtIfElse $ MkStmtIfElse
          ( RExp
          $ MkExpLogical (RExp $ ExpNum 1) OpNE (RExp $ ExpNum 1)
          )
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 100)
              , MkStmtWrite (RExp $ ExpNum 200)
              , MkStmtWrite (RExp $ ExpNum 10)
              ]
          )
          (   StmtWrite
          <$> [ MkStmtWrite (RExp $ ExpNum 999)
              , MkStmtWrite (RExp $ ExpNum 888)
              , MkStmtWrite (RExp $ ExpNum 666)
              ]
          )
        code <- getTranslatedInstrs
        return code
      )
  let simulator = Simulator.run (Simulator.initWithStdin [] code)
  Simulator.getStdout simulator @?= ["999", "888", "666"]

test_execStmtWhile :: TestTree
test_execStmtWhile = testCaseSteps "execStmtWhile" $ \step -> do
  let symbols =
        [ Symbol { symName     = "foo"
                 , symDataType = DataType [] TypeInt
                 , symDeclSpan = spanNull
                 }
        ]
  step "While stmt true"
  let code = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          execStmtAssign
            $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 0)
          execStmtWhile $ MkStmtWhile
            (RExp $ MkExpLogical (RLValue $ LValue [] "foo")
                                 OpLT
                                 (RExp $ ExpNum 10)
            )
            [ StmtAssign $ MkStmtAssign
                (LValue [] "foo")
                (RExp $ MkExpArithmetic (RLValue $ LValue [] "foo")
                                        OpAdd
                                        (RExp $ ExpNum 1)
                )
            ]
          code <- getTranslatedInstrs
          return code
        )
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "10"

  step "While stmt false"
  let code = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          execStmtAssign
            $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 100)
          execStmtWhile $ MkStmtWhile
            (RExp $ MkExpLogical (RLValue $ LValue [] "foo")
                                 OpLT
                                 (RExp $ ExpNum 10)
            )
            [ StmtAssign $ MkStmtAssign
                (LValue [] "foo")
                (RExp $ MkExpArithmetic (RLValue $ LValue [] "foo")
                                        OpAdd
                                        (RExp $ ExpNum 1)
                )
            ]
          code <- getTranslatedInstrs
          return code
        )
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "100"

test_execStmtBreak :: TestTree
test_execStmtBreak = testCaseSteps "execStmtBreak" $ \step -> do
  let symbols =
        [ Symbol { symName     = "foo"
                 , symDataType = DataType [] TypeInt
                 , symDeclSpan = spanNull
                 }
        ]
  step "Break"
  let code = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          execStmtAssign
            $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 0)
          execStmtWhile $ MkStmtWhile
            (RExp $ MkExpLogical (RLValue $ LValue [] "foo")
                                 OpLT
                                 (RExp $ ExpNum 10)
            )
            [ StmtAssign $ MkStmtAssign
              (LValue [] "foo")
              (RExp $ MkExpArithmetic (RLValue $ LValue [] "foo")
                                      OpAdd
                                      (RExp $ ExpNum 1)
              )
            , StmtBreak MkStmtBreak
            ]
          code <- getTranslatedInstrs
          return code
        )
  let simulator = Simulator.run (Simulator.init code)
  Simulator.getMemory 4096 simulator @?= "1"


test_execStmtContinue :: TestTree
test_execStmtContinue = testCaseSteps "execStmtContinue" $ \step ->
  do
    let symbols =
          [ Symbol { symName     = "foo"
                   , symDataType = DataType [] TypeInt
                   , symDeclSpan = spanNull
                   }
          , Symbol { symName     = "bar"
                   , symDataType = DataType [] TypeInt
                   , symDeclSpan = spanNull
                   }
          ]
    step "Continue"
    let (r1, code) = fromRight' $ flip
          Compiler.runCompiler
          symbols
          (do
            execStmtAssign
              $ MkStmtAssign (LValue [] "foo") (RExp $ ExpNum 0)
            execStmtAssign
              $ MkStmtAssign (LValue [] "bar") (RExp $ ExpNum 0)
            execStmtWhile $ MkStmtWhile
              (RExp $ MkExpLogical (RLValue $ LValue [] "foo")
                                   OpLT
                                   (RExp $ ExpNum 10)
              )
              [ StmtIf $ MkStmtIf
                (RExp $ MkExpLogical (RLValue $ LValue [] "foo")
                                     OpEQ
                                     (RExp $ ExpNum 5)
                )
                [ StmtAssign $ MkStmtAssign
                  (LValue [] "foo")
                  (RExp $ MkExpArithmetic
                    (RLValue $ LValue [] "foo")
                    OpAdd
                    (RExp $ ExpNum 1)
                  )
                , StmtContinue MkStmtContinue
                ]
              , StmtAssign $ MkStmtAssign
                (LValue [] "bar")
                (RExp $ MkExpArithmetic (RLValue $ LValue [] "bar")
                                        OpAdd
                                        (RLValue $ LValue [] "foo")
                )
              , StmtAssign $ MkStmtAssign
                (LValue [] "foo")
                (RExp $ MkExpArithmetic (RLValue $ LValue [] "foo")
                                        OpAdd
                                        (RExp $ ExpNum 1)
                )
              ]
            r1   <- getLValueLocInReg (LValue [] "bar")
            code <- getTranslatedInstrs
            return (r1, code)
          )
    let simulator = Simulator.run (Simulator.init code)
    let loc       = read $ Simulator.getRegVal r1 simulator
    Simulator.getMemory loc simulator
      @?= (show . sum) [0 :: Int, 1, 2, 3, 4, 6, 7, 8, 9]



----------------------------------------------------------------------------------------

test_getLValueLocInReg :: TestTree
test_getLValueLocInReg =
  testCaseSteps "getLValueLocInReg" $ \step -> do
    step "Simple"
    let symbols =
          [ Symbol { symName     = "foo"
                   , symDataType = DataType [] TypeInt
                   , symDeclSpan = spanNull
                   }
          , Symbol { symName     = "bar"
                   , symDataType = DataType [] TypeInt
                   , symDeclSpan = spanNull
                   }
          ]
    let (code, r1, r2) = fromRight' $ flip
          Compiler.runCompiler
          symbols
          (do
            r1   <- getLValueLocInReg (LValue [] "foo")
            r2   <- getLValueLocInReg (LValue [] "bar")
            code <- gets Compiler.code
            return (code, r1, r2)
          )
    let simulator = Simulator.run (Simulator.init code)
    let loc1      = Simulator.getRegVal r1 simulator
    let loc2      = Simulator.getRegVal r2 simulator
    (read loc1 >= (4096 :: Int)) @? "Variables must be in stack"
    (read loc2 >= (4096 :: Int)) @? "Variables must be in stack"
    loc1 /= loc2 @? "Two variables have different location"


    step "2D Array index"
    let symbols =
          [ -- int foo[3][4]
            Symbol { symName     = "foo"
                   , symDataType = DataType [3, 4] TypeInt
                   , symDeclSpan = spanNull
                   }
          ]
    -- foo[2][1]
    let (code, basereg, reg) = fromRight' $ flip
          Compiler.runCompiler
          symbols
          (do
            base <- getLValueLocInReg (LValue [] "foo")
            r    <- getLValueLocInReg
              (LValue (fmap (RExp . ExpNum) [2, 1]) "foo")
            code <- gets Compiler.code
            return (code, base, r)
          )
    let simulator = Simulator.run (Simulator.init code)
    let loc       = read (Simulator.getRegVal reg simulator) :: Int
    let base = read (Simulator.getRegVal basereg simulator) :: Int
    loc @?= base + 2 * 4 + 1

    step "2D Array index - 2"
    let symbols =
          [ Symbol { symName     = "bar"
                   , symDataType = DataType [3, 4] TypeInt
                   , symDeclSpan = spanNull
                   }
          -- int foo[3][4]
          , Symbol { symName     = "foo"
                   , symDataType = DataType [3, 4] TypeInt
                   , symDeclSpan = spanNull
                   }
          ]
    -- foo[2][1]
    let (code, basereg, reg) = fromRight' $ flip
          Compiler.runCompiler
          symbols
          (do
            base <- getLValueLocInReg (LValue [] "foo")
            r    <- getLValueLocInReg
              (LValue (fmap (RExp . ExpNum) [2, 1]) "foo")
            code <- gets Compiler.code
            return (code, base, r)
          )
    let simulator = Simulator.run (Simulator.init code)
    let loc       = read (Simulator.getRegVal reg simulator) :: Int
    let base = read (Simulator.getRegVal basereg simulator) :: Int
    loc @?= (base + 2 * 4 + 1)

    let symbols =
          [ Symbol { symName     = "bar"
                   , symDataType = DataType [3, 4] TypeInt
                   , symDeclSpan = spanNull
                   }
           -- int foo[10][20][7]
          , Symbol { symName     = "foo"
                   , symDataType = DataType [10, 20, 7] TypeInt
                   , symDeclSpan = spanNull
                   }
          ]
    -- foo[5][3][7]

    let (code, basereg, reg) = fromRight' $ flip
          Compiler.runCompiler
          symbols
          (do
            base <- getLValueLocInReg (LValue [] "foo")
            r    <- getLValueLocInReg
              (LValue (fmap (RExp . ExpNum) [5, 3, 7]) "foo")
            code <- gets Compiler.code
            return (code, base, r)
          )
    let simulator = Simulator.run (Simulator.init code)
    let base = read (Simulator.getRegVal basereg simulator) :: Int
    let loc       = read (Simulator.getRegVal reg simulator) :: Int
    loc @?= (base + 5 * 140 + 3 * 7 + 7 :: Int)
