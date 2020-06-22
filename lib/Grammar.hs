{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ConstraintKinds          #-}
{-# LANGUAGE FlexibleContexts         #-}
{-# LANGUAGE LambdaCase               #-}
{-# LANGUAGE PatternSynonyms          #-}
{-# LANGUAGE NamedFieldPuns          #-}
{-# LANGUAGE NamedFieldPuns          #-}

module Grammar where

import Control.Monad (unless)
import Control.Monad.Extra (unlessM)
import Data.Functor ((<&>))
import Data.Maybe (fromJust)
import Data.List (find)
import Error (Error)
import qualified Error
import Span

-- Monad

class Monad m => GrammarM m where
  gsGet :: m GrammarState
  gsPut :: GrammarState -> m ()
  gThrowError :: Error -> m a

data GrammarState =
  GrammarState
    { gsSymbolStack :: [[Symbol]]
    , gsFuncs :: [Func]
    , gsFuncContext :: Maybe (FuncDecl)
    , gsLoopStack :: Int
    , gsUserTypes :: [UserType]
    }
    deriving (Show, Eq)

gsInit :: GrammarState
gsInit = GrammarState
  { gsSymbolStack = [[]]
  , gsFuncs       = []
  , gsFuncContext = Nothing
  , gsLoopStack   = 0
  , gsUserTypes   = []
  }

-- AST


data Program =
  Program [Symbol] [FuncDef] [UserType]
  deriving (Eq, Show)

data Stmt
  = StmtAssign StmtAssign
  | StmtRead StmtRead
  | StmtWrite StmtWrite
  | StmtIf StmtIf
  | StmtIfElse StmtIfElse
  | StmtWhile StmtWhile
  | StmtBreak StmtBreak
  | StmtContinue StmtContinue
  | StmtRValue StmtRValue -- For function calls which is both an RValue and Statement
  | StmtReturn StmtReturn
  deriving (Show, Eq)

data StmtAssign
  = MkStmtAssign LValue RValue
  deriving (Show, Eq)

data StmtRead
  = MkStmtRead LValue
  deriving (Show, Eq)

data StmtWrite
  = MkStmtWrite RValue
  deriving (Show, Eq)

data StmtIf
  = MkStmtIf RValue [Stmt]
  deriving (Show, Eq)

data StmtIfElse
  = MkStmtIfElse RValue [Stmt] [Stmt]
  deriving (Show, Eq)

data StmtWhile
  = MkStmtWhile RValue [Stmt]
  deriving (Show, Eq)

data StmtBreak
  = MkStmtBreak
  deriving (Show, Eq)

data StmtContinue
  = MkStmtContinue
  deriving (Show, Eq)

data StmtReturn =
  MkStmtReturn RValue
  deriving (Show, Eq)

data StmtRValue =
  MkStmtRValue RValue
  deriving (Show, Eq)

data LValue
  = LValue
      [RValue] -- Indices, empty if simple ident
      String
  deriving (Show, Eq)

data RValue
  = RLValue LValue
  | RExp Exp
  | RFuncCall String [RValue]
  deriving (Show, Eq)

data Exp
  = ExpNum Int
  | ExpStr String
  | MkExpArithmetic RValue OpArithmetic RValue
  | MkExpRelational RValue OpRelational RValue
  | MkExpLogical RValue OpLogical RValue
  deriving (Show, Eq)

data OpArithmetic
  = OpAdd
  | OpSub
  | OpMul
  | OpDiv
  | OpMod
  deriving (Show, Eq)

data OpRelational
  = OpLT
  | OpGT
  | OpLE
  | OpGE
  | OpEQ
  | OpNE
  deriving (Show, Eq)

data OpLogical
  = OpLAnd
  | OpLOr
  deriving (Show, Eq)

data Symbol =
  Symbol
    { symName :: String
    , symType :: Type2
    , symDeclSpan :: Span
    }
  deriving (Show, Eq)

data Func
  = FuncDeclared FuncDecl
  | FuncDefined FuncDef
  deriving (Show, Eq)

data FuncDecl =
  FuncDecl
    { fDeclName :: String
    , fDeclRetType :: Type1
    , fDeclArgTypes :: [SpanW Type1]
    , fDeclSpan :: Span
    }
  deriving (Show, Eq)

data FuncDef =
  FuncDef
    { fDefName :: String
    , fDefRetType :: Type1
    , fDefArgTypes :: [SpanW Type1]
    , fDefDeclSpan :: Span
    , fDefBody :: [Stmt]
    , fDefArgsLen :: Int
    , fDefSyms :: [Symbol]
    , fDefSpan :: Span
    }
  deriving (Show, Eq)


-- Types that take single word
data Type1
  = TypeInt
  | TypeBool
  | TypeString
  | TypeUser String -- Will be implemented as pointer, so single word
  | TypeAny
  deriving (Eq)

data Type2
  = Type2
      [Int] -- Dims
      Type1
  deriving (Eq)

data UserType =
  UserType
    { utName :: String
    , utFields :: [Symbol]
    , utDeclSpan :: Span
    }
    deriving (Eq, Show)

mkProgram :: GrammarM m => m Program
mkProgram = do
  state <- gsGet
  let
    syms = case gsSymbolStack state of
      [syms] -> syms
      []     -> error $ "Should not happen - empty symbol stack"
      _      -> error $ "Should not happen - symbol stack > 1"
  funcs <- mapM getFuncDef (gsFuncs state)
  let userTypes = gsUserTypes state
  return $ Program syms funcs userTypes
 where
  getFuncDef func = case func of
    FuncDeclared f ->
      gThrowError $ errFuncNotDefined (fDeclName f) (fDeclSpan f)
    FuncDefined f -> return f

doVarDeclare :: GrammarM m => String -> Type1 -> [Int] -> Span -> m ()
doVarDeclare identName primType dims span = do
  symLookupCurCtxt identName >>= throwSymbolExists
  symInsert symbol
  return $ ()
 where
  dataType = Type2 dims primType
  symbol =
    Symbol { symName = identName, symDeclSpan = span, symType = dataType }
  throwSymbolExists maybeSym = case maybeSym of
    Nothing  -> return ()
    Just sym -> gThrowError
      $ errIdentifierRedeclared identName (symDeclSpan sym) span

mkStmtAssign :: GrammarM m => LValue -> RValue -> Span -> m StmtAssign
mkStmtAssign lhs rhs span = do
  lhsType     <- lValueType lhs
  lhsDeclSpan <- lValueDeclSpan lhs
  rhsType     <- rValueType rhs
  let (Type2 dims _) = lhsType
  unless (length dims == 0)
    $ gThrowError --
    $ errArrayNotAllowed span lhsDeclSpan
  unless (lhsType == rhsType)
    $ gThrowError --
    $ errTypeMismatch lhsType lhsDeclSpan rhsType span
  return $ MkStmtAssign lhs rhs

mkStmtRead :: GrammarM m => SpanW LValue -> m StmtRead
mkStmtRead (SpanW lValue lValueSpan) = do
  typeCheck (RLValue lValue) (Type2 [] <$> [TypeInt, TypeString]) lValueSpan
  return $ MkStmtRead lValue

mkStmtWrite :: GrammarM m => SpanW RValue -> m StmtWrite
mkStmtWrite (SpanW rValue rValueSpan) = do
  typeCheck rValue (Type2 [] <$> [TypeInt, TypeString]) rValueSpan
  return $ MkStmtWrite rValue

mkStmtIf :: GrammarM m => SpanW RValue -> [Stmt] -> m StmtIf
mkStmtIf (SpanW cond span) body = do
  typeCheck cond (Type2 [] <$> [TypeBool]) span
  return $ MkStmtIf cond body

mkStmtIfElse
  :: GrammarM m => SpanW RValue -> [Stmt] -> [Stmt] -> m StmtIfElse
mkStmtIfElse (SpanW cond span) thenBody elseBody = do
  typeCheck cond (Type2 [] <$> [TypeBool]) span
  return $ MkStmtIfElse cond thenBody elseBody

mkStmtWhile :: GrammarM m => SpanW RValue -> [Stmt] -> m StmtWhile
mkStmtWhile (SpanW cond span) body = do
  typeCheck cond (Type2 [] <$> [TypeBool]) span
  return $ MkStmtWhile cond body

mkStmtBreak :: GrammarM m => Span -> m StmtBreak
mkStmtBreak span = do
  unlessM hasLoop throwOutOfLoop
  return $ MkStmtBreak
  where throwOutOfLoop = gThrowError (Error.syntaxError span)

mkStmtContinue :: GrammarM m => Span -> m StmtContinue
mkStmtContinue span = do
  unlessM hasLoop throwOutOfLoop
  return $ MkStmtContinue
  where throwOutOfLoop = gThrowError (Error.syntaxError span)

mkStmtRValue :: Monad m => RValue -> m StmtRValue
mkStmtRValue rValue = do
  return $ MkStmtRValue rValue

mkStmtReturn :: GrammarM m => RValue -> Span -> m StmtReturn
mkStmtReturn rValue span = do
  funcDecl <- fcGet
  let retType = Type2 [] (fDeclRetType funcDecl)
  rValueType <- rValueType rValue
  unless (retType == rValueType)
    $ gThrowError --
    $ errTypeMismatch retType (fDeclSpan funcDecl) rValueType span
  return $ MkStmtReturn rValue

mkLValue :: GrammarM m => SpanW String -> [SpanW RValue] -> m LValue
mkLValue (SpanW identName identSpan) indices = do
  sym <- symLookup identName >>= \case
    Nothing  -> gThrowError $ errIdentifierNotDeclared identName identSpan
    Just sym -> return sym
  let Type2 dims _ = symType sym
  unless (length indices <= length dims)
    $ gThrowError (Error.customError "Too much indices" identSpan) -- TODO Better error
  indices' <- mapM
    (\(SpanW index indexSpan) -> do
      typeCheck index [Type2 [] TypeInt] indexSpan
      return $ index
    )
    indices
  return $ LValue indices' identName

mkExpArithmetic
  :: GrammarM m => SpanW RValue -> OpArithmetic -> SpanW RValue -> m Exp
mkExpArithmetic (SpanW r1 span1) op (SpanW r2 span2) = do
  typeCheck r1 (Type2 [] <$> [TypeInt]) span1
  typeCheck r2 (Type2 [] <$> [TypeInt]) span2
  return $ MkExpArithmetic r1 op r2

mkExpRelational
  :: GrammarM m => SpanW RValue -> OpRelational -> SpanW RValue -> m Exp
mkExpRelational (SpanW r1 span1) op (SpanW r2 span2) = do
  typeCheck r1 (Type2 [] <$> [TypeInt, TypeString]) span1
  typeCheck r2 (Type2 [] <$> [TypeInt, TypeString]) span2

  dataType1 <- rValueType r1
  dataType2 <- rValueType r2
  unless (dataType1 == dataType2)
    $ gThrowError --
    $ errTypeMismatch dataType1 span1 dataType2 span2

  return $ MkExpRelational r1 op r2

mkExpLogical
  :: GrammarM m => SpanW RValue -> OpLogical -> SpanW RValue -> m Exp
mkExpLogical (SpanW r1 span1) op (SpanW r2 span2) = do
  typeCheck r1 (Type2 [] <$> [TypeBool]) span1
  typeCheck r2 (Type2 [] <$> [TypeBool]) span2

  return $ MkExpLogical r1 op r2

doFuncDeclare
  :: GrammarM m => Type1 -> String -> [SpanW Type1] -> Span -> m ()
doFuncDeclare retType funcName argTypes span = do
  funcLookup funcName >>= throwFuncRedeclared
  let
    func = FuncDecl
      { fDeclName     = funcName
      , fDeclRetType  = retType
      , fDeclArgTypes = argTypes
      , fDeclSpan     = span
      }
  funcInsert $ FuncDeclared func
 where
  throwFuncRedeclared func = case func of
    Nothing -> return ()
    Just (FuncDeclared funcDec) ->
      gThrowError $ errFuncRedeclared span (fDeclSpan funcDec)
    Just (FuncDefined funcDef) ->
      gThrowError $ errFuncRedeclared span (fDefDeclSpan funcDef)

doFuncDefine
  :: GrammarM m
  => Type1
  -> String
  -> [SpanW (String, Type1)]
  -> Span
  -> m ([Stmt] -> m ())
doFuncDefine retType name args span = do
  fDecl <-
    funcLookup name
    >>= throwRedefined
    >>= (throwMismatch retType name args)
    >>= declareIfNotDeclared
  fcEnter fDecl
  flip mapM_ args $ \(SpanW (name, primType) span) -> symInsert $ Symbol
    { symName     = name
    , symType     = Type2 [] primType
    , symDeclSpan = span
    }
  return $ \stmts -> do
    syms <- fcExit
    let
      fDef = FuncDef
        { fDefName     = fDeclName fDecl
        , fDefRetType  = fDeclRetType fDecl
        , fDefArgTypes = fDeclArgTypes fDecl
        , fDefDeclSpan = fDeclSpan fDecl
        , fDefArgsLen  = length args
        , fDefSyms     = syms
        , fDefBody     = stmts
        , fDefSpan     = span
        }
    funcInsert (FuncDefined fDef)
 where
  throwRedefined maybeFunc = case maybeFunc of
    Nothing -> return Nothing
    Just (FuncDefined fDef) ->
      gThrowError $ errFuncRedefined span (fDefSpan fDef)
    Just (FuncDeclared fDecl) -> do
      return $ Just fDecl
  throwMismatch retType name args maybeFuncDec = case maybeFuncDec of
    Nothing      -> return Nothing
    Just (fDecl) -> do
      unless
          (and
            [ fDeclName fDecl == name
            , fDeclRetType fDecl == retType
            , (spanWVal <$> fDeclArgTypes fDecl)
              == (snd . spanWVal <$> args)
            ]
          )
        $ gThrowError
        $ errFuncDefMismatch span (fDeclSpan fDecl)
      return $ Just fDecl
  declareIfNotDeclared maybeFunc = case maybeFunc of
    Nothing -> do
      doFuncDeclare retType name (fmap (fmap snd) args) span
      func <- funcLookup name
      return $ case func of
        Just (FuncDeclared fDecl) -> fDecl
        _                         -> error "Should never happen"
    Just f -> return f

mkExpFuncCall :: GrammarM m => String -> [SpanW RValue] -> Span -> m RValue
mkExpFuncCall funcName args span = do
  fDecl <- fDeclLookup funcName >>= throwFuncNotDeclared
  unless (length args == (length $ fDeclArgTypes fDecl))
    $ gThrowError
    $ errFuncArgLengthMismatch
        (length $ fDeclArgTypes fDecl)
        (length args)
        (fDeclSpan fDecl)
        span
  args' <-
    mapM
        (\((SpanW arg argSpan), (SpanW expectedType expectedTypeSpan)) ->
          do
            argType <- rValueType arg
            let (Type2 dims argPrimType) = argType
            unless (length dims == 0 && argPrimType == expectedType)
              $ gThrowError
              $ errTypeMismatch
                  argType
                  argSpan
                  (Type2 [] expectedType)
                  expectedTypeSpan
            return arg
        )
      $ zip args (fDeclArgTypes fDecl)
  return $ RFuncCall funcName args'
 where
  throwFuncNotDeclared sym = case sym of
    Nothing  -> gThrowError $ errFuncNotDeclared funcName span
    Just sym -> return sym


doTypeDefine
  :: GrammarM m => String -> [SpanW (String, Type2)] -> Span -> m ()
doTypeDefine name fields span = do
  gsGets ((find (\t -> utName t == name)) . gsUserTypes)
    >>= throwExistingType
  let
    userType =
      UserType { utName = name, utFields = fields', utDeclSpan = span }
    fields' = map
      (\fieldSpanW ->
        let
          (symName, symType) = spanWVal fieldSpanW
          symDeclSpan        = getSpan fieldSpanW
        in Symbol { symName, symType, symDeclSpan }
      )
      fields
  gsModify (\gs -> gs { gsUserTypes = gsUserTypes gs ++ [userType] })
 where
  throwExistingType maybeType = case maybeType of
    Just userType ->
      gThrowError $ errTypeRedeclared name (utDeclSpan userType) span
    Nothing -> return ()

mkType1 :: GrammarM m => SpanW String -> m Type1
mkType1 name' =
  let
    name = spanWVal name'
    span = getSpan name'
  in case name of
    "int"  -> return TypeInt
    "str"  -> return TypeString
    "bool" -> return TypeBool
    _      -> do
      gsGets ((find (\t -> utName t == name)) . gsUserTypes)
        >>= throwTypeDoesnotExist name span
      return $ TypeUser name
 where
  throwTypeDoesnotExist name span maybeType = case maybeType of
    Just _  -> return ()
    Nothing -> gThrowError $ errTypeDoesnotExist name span


-- Helper Functions

lValueType :: GrammarM m => LValue -> m Type2
lValueType (LValue indices identName) = do
  (Type2 dims primType) <- symType . fromJust <$> symLookup identName

  let dims' = drop (length indices) dims
  return $ Type2 dims' primType

lValueDeclSpan :: GrammarM m => LValue -> m Span
lValueDeclSpan (LValue _ identName) = do
  symDeclSpan . fromJust <$> symLookup identName

rValueType :: GrammarM m => RValue -> m Type2
rValueType (RLValue v  ) = lValueType v
rValueType (RExp    exp) = return $ expType exp
rValueType (RFuncCall funcName _) =
  (Type2 []) . fDeclRetType . fromJust <$> fDeclLookup funcName

expType :: Exp -> Type2
expType exp = case exp of
  ExpNum{}          -> Type2 [] TypeInt
  ExpStr{}          -> Type2 [] TypeString
  MkExpArithmetic{} -> Type2 [] TypeInt
  MkExpRelational{} -> Type2 [] TypeBool
  MkExpLogical{}    -> Type2 [] TypeBool

-- Types

instance Show Type1 where
  show TypeInt             = "int"
  show TypeString          = "str"
  show TypeBool            = "bool"
  show (TypeUser typeName) = typeName
  show (TypeAny) = "any"

instance Show Type2 where
  show (Type2 dims primType) =
    let s = show primType
    in s ++ concatMap (\n -> "[" ++ show n ++ "]") dims



-- Utilities

gsGets :: GrammarM m => (GrammarState -> a) -> m a
gsGets f = f <$> gsGet

gsModify :: GrammarM m => (GrammarState -> GrammarState) -> m ()
gsModify f = (f <$> gsGet) >>= gsPut

insertList :: Eq a => (t -> a) -> t -> [t] -> [t]
insertList key a list = case list of
  [] -> [a]
  (a' : as') ->
    (if key a == key a' then a : as' else a' : insertList key a as')



symLookupCurCtxt :: GrammarM m => String -> m (Maybe Symbol)
symLookupCurCtxt name =
  gsGets $ (find $ \s -> symName s == name) . head . gsSymbolStack

symLookup :: GrammarM m => String -> m (Maybe Symbol)
symLookup name =
  gsGets $ (find $ \s -> symName s == name) . concat . gsSymbolStack

symInsert :: GrammarM m => Symbol -> m ()
symInsert symbol = gsModify $ \gs ->
  let
    ssHead : ssTail = gsSymbolStack gs
    ssHead'         = insertList symName symbol ssHead
    gsSymbolStack'  = ssHead' : ssTail
  in gs { gsSymbolStack = gsSymbolStack' }

funcLookup :: GrammarM m => String -> m (Maybe Func)
funcLookup name = gsGets $ (find $ \f -> funcName f == name) . gsFuncs

funcName :: Func -> String
funcName (FuncDeclared FuncDecl { fDeclName }) = fDeclName
funcName (FuncDefined  FuncDef { fDefName }  ) = fDefName

funcInsert :: GrammarM m => Func -> m ()
funcInsert func = gsModify
  $ \gs -> gs { gsFuncs = insertList (funcName) func (gsFuncs gs) }

hasLoop :: GrammarM m => m Bool
hasLoop = gsGets (\gs -> gsLoopStack gs > 0)

pushLoop :: GrammarM m => m ()
pushLoop = gsModify (\gs -> gs { gsLoopStack = gsLoopStack gs + 1 })

popLoop :: GrammarM m => m ()
popLoop =
  gsModify (\gs -> gs { gsLoopStack = max 0 (gsLoopStack gs - 1) })

fcEnter :: GrammarM m => FuncDecl -> m ()
fcEnter funcDecl = gsModify $ \s -> s
  { gsFuncContext = Just (funcDecl)
  , gsSymbolStack = [] : gsSymbolStack s
  }

fcExit :: GrammarM m => m [Symbol]
fcExit = do
  (syms, symStackTail) <- gsGets gsSymbolStack <&> \case
    [] -> error $ "Should not happen - empty symstack"
    [_x] -> error $ "Should not happen - singleton symstack"
    (syms : symStackTail) -> (syms, symStackTail)
  gsModify
    $ \s -> s { gsSymbolStack = symStackTail, gsFuncContext = Nothing }
  return syms

fcGet :: GrammarM m => m FuncDecl
fcGet = gsGets (fromJust . gsFuncContext)

fDeclLookup :: GrammarM m => String -> m (Maybe FuncDecl)
fDeclLookup name = do
  func <- funcLookup name
  case func of
    Just (FuncDeclared fDecl) -> return $ Just fDecl
    Just (FuncDefined FuncDef { fDefName, fDefRetType, fDefArgTypes, fDefDeclSpan })
      -> return $ Just $ FuncDecl
        { fDeclName     = fDefName
        , fDeclRetType  = fDefRetType
        , fDeclArgTypes = fDefArgTypes
        , fDeclSpan     = fDefDeclSpan
        }
    Nothing -> return Nothing

typeCheck :: GrammarM m => RValue -> [Type2] -> Span -> m ()
typeCheck rValue allowedTypes span = do
  dataType <- rValueType rValue
  unless (dataType `elem` allowedTypes || dataType == Type2 [] TypeAny)
    $ gThrowError --
    $ errTypeNotAllowed allowedTypes dataType span
  

-- Errors

errIdentifierNotDeclared :: String -> Span -> Error
errIdentifierNotDeclared identName span =
  [("Identifier not declared: " ++ identName, span)]

errIdentifierRedeclared :: String -> Span -> Span -> Error
errIdentifierRedeclared identName declSpan span =
  [ ("Identifier redeclared: " ++ identName, span)
  , ("Was already declared here"           , declSpan)
  ]


errTypeMismatch :: Type2 -> Span -> Type2 -> Span -> Error
errTypeMismatch identType declSpan rhsType span =
  [ ( "Type mismatch: Expected "
      ++ show identType
      ++ ". Got "
      ++ show rhsType
    , span
    )
  , ("Was declared here", declSpan)
  ]

errTypeNotAllowed :: [Type2] -> Type2 -> Span -> Error
errTypeNotAllowed allowedTypes gotType span =
  [ ( "Type not allowed: "
      ++ show gotType
      ++ ". Allowed types: "
      ++ show allowedTypes
    , span
    )
  ]

errFuncNotDeclared :: String -> Span -> Error
errFuncNotDeclared funcName span =
  [("Function not declared: " ++ funcName, span)]

errFuncNotDefined :: String -> Span -> Error
errFuncNotDefined funcName span =
  [("Function declared here, but not defined: " ++ funcName, span)]

errFuncRedeclared :: Span -> Span -> Error
errFuncRedeclared span declSpan =
  [("Function redeclared", span), ("Was already declared here", declSpan)]

errFuncRedefined :: Span -> Span -> Error
errFuncRedefined span defSpan =
  [("Function redefined", span), ("Was already defined here", defSpan)]

errFuncArgLengthMismatch :: Int -> Int -> Span -> Span -> Error
errFuncArgLengthMismatch expLength gotLength declSpan span =
  [ ( "Mismatch in function argument length. Expected: "
      ++ show expLength
      ++ ". Got: "
      ++ show gotLength
    , span
    )
  , ("Was declared here", declSpan)
  ]

errFuncDefMismatch :: Span -> Span -> Error
errFuncDefMismatch span declSpan =
  [ ("Function definition and declarations are different", span)
  , ("Was declared here", declSpan)
  ]


errArrayNotAllowed :: Span -> Span -> Error
errArrayNotAllowed span declSpan =
  [ ("An array is not allowed in this position", span)
  , ("Was declared here"                       , declSpan)
  ]

errTypeRedeclared :: String -> Span -> Span -> Error
errTypeRedeclared typeName declSpan span =
  [ ("Type redeclared: " ++ typeName, span)
  , ("Was already declared here"    , declSpan)
  ]

errTypeDoesnotExist :: String -> Span -> Error
errTypeDoesnotExist typeName span =
  [("Type doesnot exist: " ++ typeName, span)]
