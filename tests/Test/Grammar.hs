{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Test.Grammar where

import Control.Monad.Except
import Control.Monad.State.Strict
import Error (Error)
import Grammar
import Span
import Test.Tasty.HUnit
import Test.Tasty (TestTree, testGroup)
import Test.Utils

newtype TestGrammarM a =
  TestGrammarM (StateT GrammarState (Except Error) a)
  deriving ( Functor
           , Applicative
           , Monad
           , MonadState GrammarState
           , MonadError Error
           )

instance GrammarM TestGrammarM where
  gsGet       = get
  gsPut       = put
  gThrowError = throwError

gRun :: TestGrammarM a -> Either Error (a, GrammarState)
gRun = gRuns gsInit

gRuns :: GrammarState -> TestGrammarM a -> Either Error (a, GrammarState)
gRuns state (TestGrammarM stateT) = runExcept $ runStateT stateT state

gGetVals :: GrammarState -> TestGrammarM a -> IO a
gGetVals state = (fmap fst) <$> getRight . gRuns state

gGetVal :: TestGrammarM a -> IO a
gGetVal = gGetVals gsInit

gGetState :: TestGrammarM a -> IO GrammarState
gGetState = (fmap snd) <$> getRight . gRuns gsInit

gAssertRight :: TestGrammarM a -> IO ()
gAssertRight x = gGetVal x >> return ()

gAssertError :: TestGrammarM a -> IO ()
gAssertError = assertError . gRun

tests :: TestTree
tests = testGroup
  "Codegen"
  [ test_varDeclare
  , test_mkLValueSymbol
  , test_stmtAssign
  , test_stmtRead
  , test_stmtWrite
  , test_stmtIf
  , test_stmtIfElse
  , test_stmtWhile
  , test_stmtBreak
  , test_stmtContinue
  , test_funcDeclare
  , test_funcDefine
  , test_mkExpArithmetic
  , test_mkExpRelational
  , test_mkExpFuncCall
  , test_doTypeDefine
  , test_mkType1
  ]

test_varDeclare :: TestTree
test_varDeclare = testCaseSteps "Variable Declaration" $ \step -> do
  step "Declare var"
  state <- gGetState $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
  state @?= Grammar.gsInit
    { gsSymbolStack =
      [ [ Symbol
            { symName     = "foo"
            , symType     = Type2 [5, 10] TypeInt
            , symDeclSpan = span0
            }
        ]
      ]
    }

  step "Global var and function"
  state <- gGetState $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    _define <- doFuncDefine TypeInt "fn" [] span0
    _define []
  gsSymbolStack state
    @?= [ [ Symbol
              { symName     = "foo"
              , symType     = Type2 [5, 10] TypeInt
              , symDeclSpan = span0
              }
          ]
        ]
  case gsFuncs state !! 0 of
    FuncDefined FuncDef { fDefSyms } -> fDefSyms @?= []
    _ ->
      assertFailure
        $  "unexpected value of gsFuncs"
        ++ (show $ gsFuncs state)

  step "Function local var"
  state <- gGetState $ do
    _define <- doFuncDefine TypeInt "fn" [] span0
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    _define []
  gsSymbolStack state @?= [[]]
  case gsFuncs state !! 0 of
    FuncDefined FuncDef { fDefSyms } ->
      fDefSyms
        @?= [ Symbol
                { symName     = "foo"
                , symType     = Type2 [5, 10] TypeInt
                , symDeclSpan = span0
                }
            ]
    _ ->
      assertFailure
        $  "unexpected value of gsFuncs"
        ++ (show $ gsFuncs state)

  step "Redeclare var"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [1, 2] (Span 0 0)
    doVarDeclare "foo" TypeInt [2, 2] (Span 0 0)

test_mkLValueSymbol :: TestTree
test_mkLValueSymbol = testCaseSteps "mkLValueSymbol" $ \step -> do
  step "Undeclared LValue"
  gAssertError $ mkLValueSymbol (spanW "foo") []

  step "Declared LValue"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkLValueSymbol (spanW "foo") []

  step "Too many index"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [1, 2, 3] (Span 0 0)
    mkLValueSymbol (spanW "foo") (spanW . RExp . ExpNum <$> [0, 1, 0, 0])

  step "Correct index"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [5, 5] (Span 0 0)
    mkLValueSymbol (spanW "foo") (spanW . RExp . ExpNum <$> [2, 2])

test_stmtAssign :: TestTree
test_stmtAssign = testCaseSteps "StmtAssign" $ \step -> do
  step "Assign constant"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    mkStmtAssign
      (LValueSymbol "foo" (RExp . ExpNum <$> [1, 2]))
      (RExp $ ExpNum 10)
      (Span 0 0)

  step "Assign variable"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    doVarDeclare "bar" TypeInt []      (Span 0 0)
    mkStmtAssign
      (LValueSymbol "foo" (RExp . ExpNum <$> [1, 2]))
      (RLValue $ LValueSymbol "bar" [])
      (Span 0 0)

  step "Assign self"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkStmtAssign (LValueSymbol "foo" []) (RLValue $ LValueSymbol "foo" []) (Span 0 0)

  step "Type mismatch"
  gAssertError $ do
    doVarDeclare "foo" TypeString [5, 10] (Span 0 0)
    mkStmtAssign
      (LValueSymbol "foo" (RExp . ExpNum <$> [1, 2]))
      (RExp $ ExpNum 10)
      (Span 0 0)

  step "Assign to array"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    doVarDeclare "bar" TypeInt [5, 10] (Span 0 0)
    mkStmtAssign (LValueSymbol "foo" []) (RLValue $ LValueSymbol "bar" []) (Span 0 0)

test_stmtRead :: TestTree
test_stmtRead = testCaseSteps "StmtRead" $ \step -> do
  step "Read Int"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [5, 10] (Span 0 0)
    mkStmtRead $ SpanW (LValueSymbol "foo" (RExp . ExpNum <$> [0, 1])) (Span 0 0)

  step "Read String"
  gAssertRight $ do
    doVarDeclare "foo" TypeString [5, 10] (Span 0 0)
    mkStmtRead $ SpanW (LValueSymbol "foo" (RExp . ExpNum <$> [0, 1])) (Span 0 0)

  step "Read bool"
  gAssertError $ do
    doVarDeclare "foo" TypeBool [5, 10] (Span 0 0)
    mkStmtRead $ SpanW (LValueSymbol "foo" []) (Span 0 0)

  step "Read array"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [1] (Span 0 0)
    mkStmtRead $ SpanW (LValueSymbol "foo" []) (Span 0 0)

test_stmtWrite :: TestTree
test_stmtWrite = testCaseSteps "StmtWrite" $ \step -> do
  step "Write Int"
  gAssertRight $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkStmtWrite $ SpanW (RLValue $ LValueSymbol "foo" []) (Span 0 0)

  step "Write String"
  gAssertRight $ do
    doVarDeclare "foo" TypeString [] (Span 0 0)
    mkStmtWrite $ SpanW (RLValue $ LValueSymbol "foo" []) (Span 0 0)

  step "Write Bool"
  gAssertError $ do
    doVarDeclare "foo" TypeBool [] (Span 0 0)
    mkStmtWrite $ SpanW (RLValue $ LValueSymbol "foo" []) (Span 0 0)

  step "Write Array"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [1] (Span 0 0)
    mkStmtWrite $ SpanW (RLValue $ LValueSymbol "foo" []) (Span 0 0)

test_stmtIf :: TestTree
test_stmtIf = testCaseSteps "StmtIf" $ \step -> do
  step "If Bool"
  gAssertRight $ do
    doVarDeclare "foo" TypeBool [] (Span 0 0)
    mkStmtIf (SpanW (RLValue $ LValueSymbol "foo" []) (Span 0 0)) []

  step "If non Bool"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkStmtIf (SpanW (RLValue $ LValueSymbol "foo" []) (Span 0 0)) []

test_stmtIfElse :: TestTree
test_stmtIfElse = testCaseSteps "StmtIfElse" $ \step -> do
  step "IfElse Bool"
  gAssertRight $ do
    doVarDeclare "foo" TypeBool [] (Span 0 0)
    mkStmtIfElse (SpanW (RLValue $ LValueSymbol "foo" []) (Span 0 0)) [] []

  step "IfElse non Bool"
  gAssertError $ do
    doVarDeclare "foo" TypeInt [] (Span 0 0)
    mkStmtIfElse (SpanW (RLValue $ LValueSymbol "foo" []) (Span 0 0)) [] []

test_stmtWhile :: TestTree
test_stmtWhile = testCaseSteps "StmtWhile" $ \step -> do
  step "While Bool"
  gAssertRight $ do
    doVarDeclare "foo" TypeBool [] (Span 0 0)
    mkStmtWhile (SpanW (RLValue $ LValueSymbol "foo" []) (Span 0 0)) []

  step "While non Bool"
  gAssertError $ do
    doVarDeclare "foo" TypeString [] (Span 0 0)
    mkStmtWhile (SpanW (RLValue $ LValueSymbol "foo" []) (Span 0 0)) []

test_stmtBreak :: TestTree
test_stmtBreak = testCaseSteps "StmtBreak" $ \step -> do
  step "Break Inside Loop"
  gAssertRight $ do
    pushLoop
    mkStmtBreak (Span 0 0)

  step "Break Outside Loop"
  gAssertError $ mkStmtBreak (Span 0 0)

test_stmtContinue :: TestTree
test_stmtContinue = testCaseSteps "StmtContinue" $ \step -> do
  step "Continue Inside Loop"
  gAssertRight $ do
    pushLoop
    mkStmtContinue (Span 0 0)

  step "Continue Outside Loop"
  gAssertError $ mkStmtContinue (Span 0 0)


test_mkExpArithmetic :: TestTree
test_mkExpArithmetic = testCaseSteps "Exp Arithmetic" $ \step -> do
  step "Int Int"
  gAssertRight $ mkExpArithmetic
    (spanW (RExp $ ExpNum 1))
    OpAdd
    (spanW (RExp $ ExpNum 1))

  step "Str Int"
  gAssertError $ mkExpArithmetic
    (spanW (RExp $ ExpStr "Foo"))
    OpAdd
    (spanW (RExp $ ExpNum 1))

  step "Int Str"
  gAssertError $ mkExpArithmetic
    (spanW (RExp $ ExpNum 1))
    OpAdd
    (spanW (RExp $ ExpStr "Foo"))

  return ()

test_mkExpRelational :: TestTree
test_mkExpRelational = testCaseSteps "Exp Relational" $ \step -> do
  step "Int Int"
  gAssertRight $ mkExpRelational
    (spanW (RExp $ ExpNum 1))
    OpLT
    (spanW (RExp $ ExpNum 1))

  step "Str Str"
  gAssertRight $ mkExpRelational
    (spanW (RExp $ ExpStr "A"))
    OpLT
    (spanW (RExp $ ExpStr "B"))

  step "Int Str"
  gAssertError $ mkExpRelational
    (spanW (RExp $ ExpNum 1))
    OpLT
    (spanW (RExp $ ExpStr "B"))

  return ()

test_funcDeclare :: TestTree
test_funcDeclare = testCaseSteps "Func Declare" $ \step -> do
  step "Declare function"
  state <- gGetState $ do
    doFuncDeclare
      TypeInt
      "foo"
      (spanW <$> [TypeInt, TypeInt, TypeInt])
      (Span 0 0)
  state @?= Grammar.gsInit
    { gsFuncs =
      [ FuncDeclared $ FuncDecl
          { fDeclName     = "foo"
          , fDeclRetType  = TypeInt
          , fDeclArgTypes = spanW <$> [TypeInt, TypeInt, TypeInt]
          , fDeclSpan     = span0
          }
      ]
    }

  step "Redeclare function"
  gAssertError $ do
    doFuncDeclare TypeString "foo" [] (Span 0 0)
    doFuncDeclare TypeString "foo" [] (Span 0 0)

test_funcDefine :: TestTree
test_funcDefine = testCaseSteps "Func Define" $ \step -> do

  step "Declare and define function"
  state <- gGetState $ do
    doFuncDeclare TypeInt "foo" (spanW <$> [TypeInt, TypeInt]) (Span 0 0)
    define <- doFuncDefine
      TypeInt
      "foo"
      (spanW <$> [("fff", TypeInt), ("bar", TypeInt)])
      (Span 0 0)
    define []
  state @?= Grammar.gsInit
    { gsFuncs =
      [ FuncDefined $ FuncDef
          { fDefName     = "foo"
          , fDefRetType  = TypeInt
          , fDefArgTypes = spanW <$> [TypeInt, TypeInt]
          , fDefDeclSpan = span0
          , fDefBody     = []
          , fDefArgsLen  = 2
          , fDefSyms     =
            [ Symbol
              { symName     = "fff"
              , symType     = Type2 [] TypeInt
              , symDeclSpan = span0
              }
            , Symbol
              { symName     = "bar"
              , symType     = Type2 [] TypeInt
              , symDeclSpan = span0
              }
            ]
          , fDefSpan     = span0
          }
      ]
    }

  step "Define without declare"
  state <- gGetState $ do
    define <- doFuncDefine
      TypeInt
      "foo"
      (flip SpanW (Span 0 0) <$> [("fff", TypeInt), ("bar", TypeInt)])
      (Span 0 0)
    doVarDeclare "asd" TypeInt [] span0
    define []
  state @?= Grammar.gsInit
    { gsFuncs =
      [ FuncDefined $ FuncDef
          { fDefName     = "foo"
          , fDefRetType  = TypeInt
          , fDefArgTypes = spanW <$> [TypeInt, TypeInt]
          , fDefDeclSpan = span0
          , fDefBody     = []
          , fDefArgsLen  = 2
          , fDefSyms     =
            [ Symbol
              { symName     = "fff"
              , symType     = Type2 [] TypeInt
              , symDeclSpan = span0
              }
            , Symbol
              { symName     = "bar"
              , symType     = Type2 [] TypeInt
              , symDeclSpan = span0
              }
            , Symbol
              { symName     = "asd"
              , symType     = Type2 [] TypeInt
              , symDeclSpan = span0
              }
            ]
          , fDefSpan     = span0
          }
      ]
    }

  step "Redeclare function"
  gAssertError $ do
    define <- doFuncDefine
      TypeInt
      "foo"
      (flip SpanW (Span 0 0) <$> [("fff", TypeInt), ("bar", TypeInt)])
      (Span 0 0)
    define []
    doFuncDeclare TypeString "foo" [] (Span 0 0)

  step "Function declaration mismatch - return type"
  gAssertError $ do
    doFuncDeclare TypeString "foo" [] (Span 0 0)
    _ <- doFuncDefine TypeInt "foo" [] (Span 0 0)
    return ()

  step "Function declaration mismatch - args"
  gAssertError $ do
    doFuncDeclare TypeString "foo" [] (Span 0 0)
    _ <- doFuncDefine
      TypeString
      "foo"
      [SpanW ("ff", TypeInt) (Span 0 0)]
      (Span 0 0)
    return ()

test_mkExpFuncCall :: TestTree
test_mkExpFuncCall = testCaseSteps "mkExpFuncCall" $ \step -> do
  step "Undeclared function"
  gAssertError $ do
    mkExpFuncCall "foo" [] (Span 0 0)

  step "Declared function"
  gAssertRight $ do
    doFuncDeclare TypeInt "foo" [] (Span 0 0)
    mkExpFuncCall "foo" [] (Span 0 0)

  step "arg type mismatch"
  gAssertError $ do
    doFuncDeclare TypeInt "foo" (spanW <$> [TypeInt]) (Span 0 0)
    mkExpFuncCall "foo" (spanW . RExp . ExpNum <$> [1, 2]) (Span 0 0)

  step "assign to var"
  gAssertRight $ do
    doVarDeclare "bar" TypeInt [] (Span 0 0)
    doFuncDeclare TypeInt "foo" [] (Span 0 0)
    exp    <- mkExpFuncCall "foo" [] (Span 0 0)
    lValue <- mkLValueSymbol (spanW "bar") []
    mkStmtAssign lValue exp (Span 0 0)

  return ()

test_doTypeDefine :: TestTree
test_doTypeDefine = testCaseSteps "Type Define" $ \step -> do
  step "New type"
  state <- gGetState $ do
    doTypeDefine
      "myType"
      [spanW ("foo", Type2 [] TypeInt), spanW ("bar", Type2 [2] TypeInt)]
      span0
    doTypeDefine
      "myType2"
      [spanW ("foo", Type2 [] TypeInt), spanW ("bar", Type2 [2] TypeInt)]
      span0

  state @?= gsInit
    { gsUserTypes =
      [ UserType
        { utName     = "myType"
        , utFields   =
          [ Symbol
            { symName     = "foo"
            , symType     = Type2 [] TypeInt
            , symDeclSpan = span0
            }
          , Symbol
            { symName     = "bar"
            , symType     = Type2 [2] TypeInt
            , symDeclSpan = span0
            }
          ]
        , utDeclSpan = span0
        }
      , UserType
        { utName     = "myType2"
        , utFields   =
          [ Symbol
            { symName     = "foo"
            , symType     = Type2 [] TypeInt
            , symDeclSpan = span0
            }
          , Symbol
            { symName     = "bar"
            , symType     = Type2 [2] TypeInt
            , symDeclSpan = span0
            }
          ]
        , utDeclSpan = span0
        }
      ]
    }

  step "Redeclare type"
  gAssertError $ do
    doTypeDefine
      "myType"
      [spanW ("foo", Type2 [] TypeInt), spanW ("bar", Type2 [2] TypeInt)]
      span0
    doTypeDefine
      "myType"
      [spanW ("foo", Type2 [] TypeInt), spanW ("bar", Type2 [2] TypeInt)]
      span0

test_mkType1 :: TestTree
test_mkType1 = testCaseSteps "mkType1" $ \step -> do
  step "Existing type"
  t <- gGetVals
    (gsInit
      { gsUserTypes =
        [ UserType
            { utName     = "foo"
            , utFields   =
              [ Symbol
                  { symName     = "fooa"
                  , symType     = Type2 [] TypeInt
                  , symDeclSpan = span0
                  }
              ]
            , utDeclSpan = span0
            }
        ]
      }
    )
    (do
      mkType1 (spanW "foo")
    )
  t @?= TypeUser "foo"

  step "Non existing type"
  gAssertError $ do
    mkType1 (spanW "foo")
