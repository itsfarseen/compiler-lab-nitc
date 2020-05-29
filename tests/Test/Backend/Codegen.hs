{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module Test.Backend.Codegen where

import Test.Tasty.HUnit
import Control.Monad.State.Strict
import Data.Either.Extra

import Grammar
import Span
import Backend.Codegen
import qualified Backend.Compiler as Compiler
import qualified Backend.Simulator as Simulator

spanNull = Span 0 0

unit_getLValueLocInReg_Simple = do
  let symbols =
        [ Symbol { symName     = "foo"
                 , symDataType = DataTypeInt
                 , symDeclSpan = spanNull
                 }
        , Symbol { symName     = "bar"
                 , symDataType = DataTypeInt
                 , symDeclSpan = spanNull
                 }
        ]
  let (code, r1, r2) = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          r1   <- getLValueLocInReg (LValueIdent $ MkIdent "foo")
          r2   <- getLValueLocInReg (LValueIdent $ MkIdent "bar")
          code <- gets Compiler.code
          return (code, r1, r2)
        )
  let simulator = Simulator.run (Simulator.init code)
  let loc1      = Simulator.getRegVal r1 simulator
  let loc2      = Simulator.getRegVal r2 simulator
  loc1 @?= 4096
  loc2 @?= 4097


unit_getLValueLocInReg_Array = do
  let symbols =
        [ -- int foo[3][4]
          Symbol { symName     = "foo"
                 , symDataType = DataTypeArray 3 (DataTypeArray 4 DataTypeInt)
                 , symDeclSpan = spanNull
                 }
        ]
  -- foo[2][1]
  let lValue = LValueArrayIndex
        (Exp (ExpNum 2))
        (LValueArrayIndex (Exp (ExpNum 1)) (LValueIdent (MkIdent "foo")))

  let (code, reg) = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          r    <- getLValueLocInReg lValue
          code <- gets Compiler.code
          return (code, r)
        )
  let simulator = Simulator.run (Simulator.init code)
  let loc       = Simulator.getRegVal reg simulator
  loc @?= 4096 + 2 * 4 + 1

unit_getLValueLocInReg_Array_2 = do
  let symbols =
        [ Symbol { symName     = "bar"
                 , symDataType = DataTypeArray 3 (DataTypeArray 4 DataTypeInt)
                 , symDeclSpan = spanNull
                 }
        -- int foo[3][4]
        , Symbol { symName     = "foo"
                 , symDataType = DataTypeArray 3 (DataTypeArray 4 DataTypeInt)
                 , symDeclSpan = spanNull
                 }
        ]
  -- foo[2][1]
  let lValue = LValueArrayIndex
        (Exp (ExpNum 2))
        (LValueArrayIndex (Exp (ExpNum 1)) (LValueIdent (MkIdent "foo")))

  let (code, reg) = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          r    <- getLValueLocInReg lValue
          code <- gets Compiler.code
          return (code, r)
        )
  let simulator = Simulator.run (Simulator.init code)
  let loc       = Simulator.getRegVal reg simulator
  loc @?= 4096 + (3 * 4) + 2 * 4 + 1



unit_getLValueLocInReg_Array_3D = do
  let symbols =
        [ Symbol { symName     = "bar"
                 , symDataType = DataTypeArray 3 (DataTypeArray 4 DataTypeInt)
                 , symDeclSpan = spanNull
                 }
         -- int foo[10][20][7]
        , Symbol
          { symName     = "foo"
          , symDataType = DataTypeArray 10
                          $ DataTypeArray 20
                          $ DataTypeArray 7
                          $ DataTypeInt
          , symDeclSpan = spanNull
          }
        ]
  -- foo[5][3][7]
  let lValue = LValueArrayIndex
        (Exp (ExpNum 5))
        (LValueArrayIndex
          (Exp (ExpNum 3))
          (LValueArrayIndex (Exp (ExpNum 7)) (LValueIdent (MkIdent "foo")))
        )

  let (code, reg) = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          r    <- getLValueLocInReg lValue
          code <- gets Compiler.code
          return (code, r)
        )
  let simulator = Simulator.run (Simulator.init code)
  let loc       = Simulator.getRegVal reg simulator
  loc @?= 4096 + (3 * 4) + 5 * 140 + 3 * 7 + 7



unit_assign = do
  let symbols =
        [ Symbol { symName     = "bar"
                 , symDataType = DataTypeArray 3 (DataTypeArray 4 DataTypeInt)
                 , symDeclSpan = spanNull
                 }
        ]
  let lhs = LValueArrayIndex
        (Exp $ ExpNum 1)
        (LValueArrayIndex (Exp $ ExpNum 2) (LValueIdent (MkIdent "bar")))

  let rhs = (Exp $ ExpNum 100)
  let code = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          execStmtAssign (MkStmtAssign lhs rhs)
          gets Compiler.code
        )
  let simulator = Simulator.run (Simulator.init code)
  let loc       = 4096 + 1 * 4 + 2
  let val       = Simulator.getMemory loc simulator
  val @?= 100


unit_Index_Using_Variable = do
  let symbols =
        [ Symbol { symName     = "bar"
                 , symDataType = DataTypeArray 3 (DataTypeArray 4 DataTypeInt)
                 , symDeclSpan = spanNull
                 }
        , Symbol { symName     = "i"
                 , symDataType = DataTypeInt
                 , symDeclSpan = spanNull
                 }
        , Symbol { symName     = "j"
                 , symDataType = DataTypeInt
                 , symDeclSpan = spanNull
                 }
        ]
  let lhs = LValueArrayIndex
        (LValue $ LValueIdent (MkIdent "i"))
        (LValueArrayIndex (LValue $ LValueIdent (MkIdent "j"))
                          (LValueIdent (MkIdent "bar"))
        )

  let rhs = (Exp $ ExpNum 100)
  let code = fromRight' $ flip
        Compiler.runCompiler
        symbols
        (do
          execStmtAssign
            (MkStmtAssign (LValueIdent (MkIdent "i")) (Exp $ ExpNum 1))
          execStmtAssign
            (MkStmtAssign (LValueIdent (MkIdent "j")) (Exp $ ExpNum 2))
          execStmtAssign (MkStmtAssign lhs rhs)
          gets Compiler.code
        )
  let simulator = Simulator.run (Simulator.init code)
  let loc       = 4096 + 1 * 4 + 2
  let val       = Simulator.getMemory loc simulator
  val @?= 100

