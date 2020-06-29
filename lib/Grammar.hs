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
import Data.Function ((&))
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
  | StmtRValue StmtRValue -- For function calls, syscall
                          -- they are both RValue and Statement
  | StmtReturn StmtReturn
  | StmtInitialize StmtInitialize
  | StmtAlloc StmtAlloc
  | StmtFree StmtFree
  | StmtPoke StmtPoke
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

data StmtRValue =
  MkStmtRValue RValue
  deriving (Show, Eq)

data StmtReturn =
  MkStmtReturn RValue
  deriving (Show, Eq)

data StmtInitialize =
  MkStmtInitialize
  deriving (Show, Eq)

data StmtAlloc =
  MkStmtAlloc LValue
  deriving (Show, Eq)

data StmtFree =
  MkStmtFree LValue
  deriving (Show, Eq)

data StmtPoke =
  MkStmtPoke RValue RValue
  deriving (Show, Eq)

data LValue
  = LValueSymbol String [RValue]
  | LValueField LValue String [RValue]
  deriving (Show, Eq)

data RValue
  = RLValue LValue
  | RExp Exp
  | RFuncCall String [RValue]
  | RSyscall Int Int RValue RValue RValue
  | RPeek RValue
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
    , symVisibility :: SymbolVisibility
    }
  deriving (Show, Eq)

data SymbolVisibility
  = SVPublic
  | SVPrivate
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

mkProgram :: GrammarState -> Either Error Program
mkProgram state = do
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
      Left $ errFuncNotDefined (fDeclName f) (fDeclSpan f)
    FuncDefined f -> return f


-- TODO - Change [Symbol] to (GrammarState -> [Symbol]) -> GrammarState
doVarDeclare :: String -> Type1 -> [Int] -> Span -> [Symbol] -> Either Error [Symbol] 
doVarDeclare identName primType dims span symbols = do
  checkSymbolExists
  return $ symInsert symbol symbols
 where
  dataType = Type2 dims primType
  symbol   = Symbol
    { symName       = identName
    , symDeclSpan   = span
    , symType       = dataType
    , symVisibility = SVPublic
    }
  checkSymbolExists = case symLookup' identName symbols of
    Nothing  -> return ()
    Just sym -> Left
      $ errIdentifierRedeclared identName (symDeclSpan sym) span

mkStmtAssign :: LValue -> RValue -> Span -> GrammarState -> Either Error StmtAssign 
mkStmtAssign lhs rhs span state = do
  let lhsType =     lValueType lhs state
  let lhsDeclSpan = lValueDeclSpan lhs state
  let rhsType   = rValueType rhs state
  let (Type2 dims _) = lhsType
  unless (length dims == 0)
    $ Left --
    $ errArrayNotAllowed span lhsDeclSpan
  unless
      (  lhsType
      == rhsType
      || lhsType
      == Type2 [] TypeAny
      || rhsType
      == Type2 [] TypeAny
      )
    $ Left --
    $ errTypeMismatch lhsType lhsDeclSpan rhsType span
  return $ MkStmtAssign lhs rhs

mkStmtRead :: SpanW LValue -> GrammarState -> Either Error StmtRead
mkStmtRead (SpanW lValue lValueSpan) state = do
  typeCheck
    (RLValue lValue)
    (Type2 [] <$> [TypeInt, TypeString])
    lValueSpan
    state
  return $ MkStmtRead lValue

mkStmtWrite :: SpanW RValue -> GrammarState -> Either Error StmtWrite
mkStmtWrite (SpanW rValue rValueSpan) state = do
  typeCheck rValue (Type2 [] <$> [TypeInt, TypeString]) rValueSpan state
  return $ MkStmtWrite rValue

mkStmtIf :: SpanW RValue -> [Stmt] -> GrammarState -> Either Error StmtIf
mkStmtIf (SpanW cond span) body state = do
  typeCheck cond (Type2 [] <$> [TypeBool]) span state
  return $ MkStmtIf cond body

mkStmtIfElse
  :: SpanW RValue -> [Stmt] -> [Stmt] -> GrammarState -> Either Error StmtIfElse
mkStmtIfElse (SpanW cond span) thenBody elseBody state = do
  typeCheck cond (Type2 [] <$> [TypeBool]) span state
  return $ MkStmtIfElse cond thenBody elseBody

mkStmtWhile :: SpanW RValue -> [Stmt] -> GrammarState -> Either Error StmtWhile 
mkStmtWhile (SpanW cond span) body state = do
  typeCheck cond (Type2 [] <$> [TypeBool]) span state
  return $ MkStmtWhile cond body

mkStmtBreak :: Span -> GrammarState -> Either Error StmtBreak
mkStmtBreak span state = do
  unless (hasLoop state) throwOutOfLoop
  return $ MkStmtBreak
  where throwOutOfLoop = Left (Error.syntaxError span)

mkStmtContinue :: Span -> GrammarState -> Either Error StmtContinue
mkStmtContinue span state = do
  unless (hasLoop state) throwOutOfLoop
  return $ MkStmtContinue
  where throwOutOfLoop = Left (Error.syntaxError span)

mkStmtRValue :: RValue -> StmtRValue
mkStmtRValue rValue = do
  MkStmtRValue rValue

mkStmtReturn :: RValue -> Span -> GrammarState -> Either Error StmtReturn
mkStmtReturn rValue span state = do
  funcDecl <- throwNotInFunction $ gsFuncContext state
  let retType = Type2 [] (fDeclRetType funcDecl)
  let rValueType_ = rValueType rValue state
  unless (retType == rValueType_)
    $ Left --
    $ errTypeMismatch retType (fDeclSpan funcDecl) rValueType_ span
  return $ MkStmtReturn rValue
    where throwNotInFunction maybeFuncDecl = case maybeFuncDecl of
                                               Just funcDecl -> return funcDecl
                                               Nothing -> Left $ Error.customError "Return outside function." span

mkStmtInitialize :: Span -> GrammarState -> Either Error StmtInitialize
mkStmtInitialize _span _state = do
  -- TODO Check for double initialization
  return $ MkStmtInitialize

mkStmtAlloc :: LValue -> Span -> GrammarState -> Either Error StmtAlloc
mkStmtAlloc lValue span state = do
  let (Type2 dims type1) = lValueType lValue state
  let declSpan           = lValueDeclSpan lValue state
  unless (dims == [])
    $ Left --
    $ errArrayNotAllowed span declSpan
  unless
      (case type1 of
        TypeUser{} -> True
        _          -> False
      )
    $ Left --
    $ errExpectedUserType type1 span declSpan
  return $ MkStmtAlloc lValue

mkStmtFree ::  LValue -> Span -> GrammarState -> Either Error StmtFree
mkStmtFree lValue span state = do
  let (Type2 dims type1) = lValueType lValue state
  let declSpan           = lValueDeclSpan lValue state
  unless (dims == [])
    $ Left --
    $ errArrayNotAllowed span declSpan
  unless
      (case type1 of
        TypeUser{} -> True
        _          -> False
      )
    $ Left --
    $ errExpectedUserType type1 span declSpan
  return $ MkStmtFree lValue

mkStmtPoke
  ::  SpanW RValue -> SpanW RValue -> Span -> GrammarState -> Either Error StmtPoke
mkStmtPoke rValue1SW rValue2SW span state = do
  let rValue1 = spanWVal rValue1SW
  let rValue2 = spanWVal rValue2SW
  typeCheck rValue1 [Type2 [] TypeInt] (getSpan rValue1SW) state
  let (Type2 dims2 _) = rValueType rValue2 state
  unless (dims2 == [])
    $ Left --
    $ errArrayNotAllowed span span
  return $ MkStmtPoke rValue1 rValue2



mkLValueSymbol :: SpanW String -> [SpanW RValue] -> GrammarState -> Either Error LValue
mkLValueSymbol (SpanW identName identSpan) indices state = do
  sym <- symLookup identName state & \case
    Nothing  -> gThrowError $ errIdentifierNotDeclared identName identSpan
    Just sym -> return sym
  let Type2 dims _ = symType sym
  unless (length indices <= length dims)
    $ Left (Error.customError "Too much indices" identSpan) -- TODO Better error
  indices' <- mapM
    (\(SpanW index indexSpan) -> do
      typeCheck index [Type2 [] TypeInt] indexSpan state
      return $ index
    )
    indices
  return $ LValueSymbol identName indices'

mkLValueField
  ::  LValue -> SpanW String -> [SpanW RValue] -> GrammarState -> Either Error LValue
mkLValueField lValue (SpanW fieldName fieldNameSpan) indices state = do
  utName <- getUserTypeName $ lValueType lValue state
  let userType = fromJust $ userTypeLookup utName (gsUserTypes state)
  sym    <- symLookup' fieldName (utFields userType) & \case
    Nothing ->
      Left $ errIdentifierNotDeclared fieldName fieldNameSpan -- TODO Better error
    Just sym -> return sym
  let Type2 dims _ = symType sym
  unless (length indices <= length dims)
    $ Left (Error.customError "Too much indices" fieldNameSpan) -- TODO Better error
  indices' <- mapM
    (\(SpanW index indexSpan) -> do
      typeCheck index [Type2 [] TypeInt] indexSpan state
      return $ index
    )
    indices
  return $ LValueField lValue fieldName indices'
 where
  getUserTypeName typ = case typ of
    Type2 [] (TypeUser utName) -> return utName
    Type2 _  _                 -> Left
      (Error.customError
        ("Cannot find field in type: " ++ (show typ))
        fieldNameSpan
      ) -- TODO Better error


mkExpArithmetic
  ::  SpanW RValue -> OpArithmetic -> SpanW RValue -> GrammarState -> Either Error Exp
mkExpArithmetic (SpanW r1 span1) op (SpanW r2 span2)  state= do
  typeCheck r1 (Type2 [] <$> [TypeInt]) span1 state
  typeCheck r2 (Type2 [] <$> [TypeInt]) span2 state
  return $ MkExpArithmetic r1 op r2

mkExpRelational
  :: SpanW RValue -> OpRelational -> SpanW RValue -> GrammarState -> Either Error Exp
mkExpRelational (SpanW r1 span1) op (SpanW r2 span2) state = do
  let dataType1 = rValueType r1 state
  let dataType2 = rValueType r2 state
  let (Type2 dims1 _) = dataType1
  let (Type2 dims2 _) = dataType2
  unless (dims1 == [])
    $ Left --
    $ errArrayNotAllowed span1 span1 -- TODO DeclSpan

  unless (dims2 == [])
    $ Left --
    $ errArrayNotAllowed span2 span2 -- TODO DeclSpan

  unless
      (  dataType1
      == Type2 [] TypeAny
      || dataType2
      == Type2 [] TypeAny
      || dataType1
      == dataType2
      )
    $ Left --
    $ errTypeMismatch dataType1 span1 dataType2 span2

  return $ MkExpRelational r1 op r2

mkExpLogical
  :: SpanW RValue -> OpLogical -> SpanW RValue -> GrammarState -> Either Error Exp
mkExpLogical (SpanW r1 span1) op (SpanW r2 span2) state = do
  typeCheck r1 (Type2 [] <$> [TypeBool]) span1 state
  typeCheck r2 (Type2 [] <$> [TypeBool]) span2 state

  return $ MkExpLogical r1 op r2

doFuncDeclare
  :: Type1 -> String -> [SpanW Type1] -> Span -> [Func] -> Either Error [Func]
doFuncDeclare retType funcName argTypes span funcs = do
  funcLookup funcName funcs & throwFuncRedeclared
  let
    func = FuncDecl
      { fDeclName     = funcName
      , fDeclRetType  = retType
      , fDeclArgTypes = argTypes
      , fDeclSpan     = span
      }
  return $ funcInsert (FuncDeclared func) funcs
 where
  throwFuncRedeclared func = case func of
    Nothing -> return ()
    Just (FuncDeclared funcDec) ->
      gThrowError $ errFuncRedeclared span (fDeclSpan funcDec)
    Just (FuncDefined funcDef) ->
      gThrowError $ errFuncRedeclared span (fDefDeclSpan funcDef)

doFuncDefine
  :: 
   Type1
  -> String
  -> [SpanW (String, Type1)]
  -> Span
  -> [Func] -> ([Func], ([Stmt] -> [Func]))
doFuncDefine retType name args span funcs = do
  fDecl <-
    funcLookup name funcs
    & throwRedefined
    & (throwMismatch retType name args)
    & declareIfNotDeclared
  fcEnter fDecl
  flip mapM_ args $ \(SpanW (name, primType) span) -> symInsert $ Symbol
    { symName       = name
    , symType       = Type2 [] primType
    , symDeclSpan   = span
    , symVisibility = SVPublic
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
    funcInsert (FuncDefined fDef) funcs
 where
  throwRedefined maybeFunc = case maybeFunc of
    Nothing -> return Nothing
    Just (FuncDefined fDef) ->
      Left $ errFuncRedefined span (fDefSpan fDef)
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
        $ Left
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

mkExpFuncCall :: String -> [SpanW RValue] -> Span -> GrammarState -> RValue
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
            unless
                (  length dims
                == 0
                && (  argPrimType
                   == expectedType
                   || argPrimType
                   == TypeAny
                   || expectedType
                   == TypeAny
                   )
                )
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
  :: 
    SpanW String
  -> [UserType] -> ([UserType], [SpanW (String, Type2)] -> Span -> [UserType])
doTypeDefine (SpanW name nameSpan) = do
  gsGets ((find (\t -> utName t == name)) . gsUserTypes)
    >>= throwExistingType
  let
    userType =
      UserType { utName = name, utFields = [], utDeclSpan = nameSpan }
  gsModify (\gs -> gs { gsUserTypes = gsUserTypes gs ++ [userType] })
  return $ \fields span -> do
    let
      userType =
        UserType { utName = name, utFields = fields', utDeclSpan = span }
      fields' = map
        (\fieldSpanW ->
          let
            (symName, symType) = spanWVal fieldSpanW
            symDeclSpan        = getSpan fieldSpanW
          in Symbol
            { symName
            , symType
            , symDeclSpan
            , symVisibility = SVPublic
            }
        )
        fields
    gsModify
      (\gs -> gs
        { gsUserTypes = map
          (\ut -> if utName ut == name then userType else ut)
          (gsUserTypes gs)
        }
      )
 where
  throwExistingType maybeType = case maybeType of
    Just userType ->
      gThrowError $ errTypeRedeclared name (utDeclSpan userType) nameSpan
    Nothing -> return ()

mkType1 :: SpanW String -> GrammarState -> Type1
mkType1 name' =
  let
    name = spanWVal name'
    span = getSpan name'
  in case name of
    "int"  -> return TypeInt
    "str"  -> return TypeString
    "bool" -> return TypeBool
    "any"  -> return TypeAny
    _      -> do
      userTypeLookup name >>= throwTypeDoesnotExist name span
      return $ TypeUser name
 where
  throwTypeDoesnotExist name span maybeType = case maybeType of
    Just _  -> return ()
    Nothing -> gThrowError $ errTypeDoesnotExist name span


mkExpSyscall
  :: SpanW Int
  -> SpanW Int
  -> SpanW RValue
  -> SpanW RValue
  -> SpanW RValue
  -> Span
  -> GrammarState -> RValue
mkExpSyscall intNum callNum arg1 arg2 arg3 _span = do
  return $ RSyscall
    (spanWVal intNum)
    (spanWVal callNum)
    (spanWVal arg1)
    (spanWVal arg2)
    (spanWVal arg3)

mkExpPeek :: SpanW RValue -> GrammarState -> RValue
mkExpPeek rValueSW = do
  let
    rValue = spanWVal rValueSW
    span   = getSpan rValueSW
  typeCheck rValue [Type2 [] TypeInt] span
  return $ RPeek rValue
-- Helper Functions


lValueType :: LValue -> GrammarState -> Type2
lValueType (LValueSymbol identName indices) state = 
  let sym = fromJust $ symLookup identName state
      (Type2 dims primType) = symType sym
      dims'                 = drop (length indices) dims
  in Type2 dims' primType
lValueType (LValueField lValue identName indices) state = 
  let parentType = lValueType lValue state
  
      utName = case parentType of
        Type2 [] (TypeUser utName) -> utName
        _                          -> error $ "Unreachable"
      sym = fromJust $ symLookup' identName (utFields $ fromJust $ userTypeLookup utName (gsUserTypes state))
      (Type2 dims primType) = symType sym
      dims'                 = drop (length indices) dims
  in Type2 dims' primType

lValueDeclSpan :: LValue -> GrammarState -> Span
lValueDeclSpan (LValueSymbol identName _) state = 
  symDeclSpan . fromJust $ symLookup identName state
lValueDeclSpan (LValueField lValue identName _) state = 
  let parentType = lValueType lValue
      utName = case parentType of
        Type2 [] (TypeUser utName) -> utName
        _                          -> error $ "Unreachable"
      sym = fromJust $ symLookup' identName (utFields $ fromJust $ userTypeLookup utName (gsUserTypes state))
  in symDeclSpan sym

rValueType :: RValue -> GrammarState -> Type2
rValueType (RLValue v  ) = lValueType v
rValueType (RExp    exp) = return $ expType exp
rValueType (RSyscall{} ) = return (Type2 [] TypeAny)
rValueType (RPeek{}    ) = return (Type2 [] TypeAny)
rValueType (RFuncCall funcName _) =
  (Type2 []) . fDeclRetType . fromJust <$> fDeclLookup funcName

expType :: Exp -> GrammarState -> Type2
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
  show (TypeAny          ) = "any"

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

symLookup :: String -> GrammarState -> (Maybe Symbol)
symLookup name =
  (find $ \s -> symName s == name) . concat . gsSymbolStack

symLookup' :: String -> [Symbol] -> Maybe Symbol
symLookup' name symbols = 
  (find $ \s -> symName s == name) symbols

symInsert :: Symbol -> [Symbol] -> [Symbol]
symInsert = insertList symName 

funcLookup :: String -> [Func] -> (Maybe Func)
funcLookup name = (find $ \f -> funcName f == name)

userTypeLookup :: String -> [UserType] -> Maybe UserType
userTypeLookup name = find (\t -> utName t == name)

funcName :: Func -> String
funcName (FuncDeclared FuncDecl { fDeclName }) = fDeclName
funcName (FuncDefined  FuncDef { fDefName }  ) = fDefName

funcInsert :: Func -> [Func] -> [Func]
funcInsert = 
  insertList (funcName)

hasLoop :: GrammarState -> Bool
hasLoop state = gsLoopStack state > 0

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

typeCheck :: RValue -> [Type2] -> Span -> GrammarState -> Either Error ()
typeCheck rValue allowedTypes span state = do
  let dataType = rValueType rValue state
  unless (dataType `elem` allowedTypes || dataType == Type2 [] TypeAny)
    $ Left --
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

errExpectedUserType :: Type1 -> Span -> Span -> Error
errExpectedUserType t span declSpan =
  [ ("Expected user type. Actual type: " ++ (show t), span)
  , ("Was declared here"                            , declSpan)
  ]
