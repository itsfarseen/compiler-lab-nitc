{
-- vim:ft=happy:
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}
module Parser where

import qualified Lexer
import Frontend
import Grammar
import Token
import Span
import Error (Error)
import qualified Error

import Text.Printf
import Data.Functor
import Data.Function ((&))
import Data.Tuple.Extra
import Debug.Trace
import Utils
import GrammarUtils
import Data.Maybe (maybeToList)
import Control.Monad ((>=>), unless)

}

%name parse
%monad {Frontend}
%lexer {Lexer.lexer} {TokenEOF _}
%tokentype {Token}
%error {parseError}

%token
    number     { TokenNumber    $$ }
    strlit     { TokenStrLit    $$ }
    ident      { TokenIdent     $$ }

    '+'        { TokenPlus      _ }
    '-'        { TokenMinus     _ }
    '*'        { TokenStar      _ }
    '/'        { TokenFwdSlash  _ }
    '%'        { TokenPercent   _ }

    '('        { TokenParOpen   _ }
    ')'        { TokenParClose  _ }

    '{'        { TokenBraceOpen _ }
    '}'        { TokenBraceClose _}

    '['        { TokenSqBracketOpen _ }
    ']'        { TokenSqBracketClose _}

    ';'        { TokenSemiColon _ }
    ','        { TokenComma     _ }
    '.'        { TokenDot       _ }
    '='        { TokenEquals    _ }

    '<'        { TokenLT        _ }
    '=='       { TokenEQ        _ }
    '!='       { TokenNE        _ }
    '>'        { TokenGT        _ }
    '<='       { TokenLE        _ }
    '>='       { TokenGE        _ }

    '&&'       { TokenLAnd      _ }
    '||'       { TokenLOr       _ }

    read       { TokenRead      _ }
    write      { TokenWrite     _ }

    if         { TokenIf        _ }
    then       { TokenThen      _ }
    else       { TokenElse      _ }
    endif      { TokenEndIf     _ }
    while      { TokenWhile     _ }
    endwhile   { TokenEndWhile  _ }
    do         { TokenDo        _ }
    break      { TokenBreak     _ }
    continue   { TokenContinue  _ }
    return     { TokenReturn    _ }

    type       { TokenType       _ }
    syscall    { TokenSyscall    _ }
    initialize { TokenInitialize _ }
    alloc      { TokenAlloc      _ }
    free       { TokenFree       _ }
    peek       { TokenPeek       _ }
    poke       { TokenPoke       _ }
    class      { TokenClass      _ }
    extends    { TokenExtends    _ }
    null       { TokenNull       _ }
    new        { TokenNew        _ }

%nonassoc '='
%left '&&' '||'
%nonassoc '==' '<' '>' '<=' '>=' '!='
%left '+' '-'
%left '*' '/' '%'

%%

Program :: { Either Error Program }
Program
    : TSList            
    { do
        gState <- $1 gStateInit
        let gSyms = gsSyms gState
        let funcs = gsFuncs gState
        let userTypes = gsUserTypes gState
        mkProgram gSyms funcs userTypes 
    }

TSList :: { GState -> Either Error GState }
TSList 
    : TopLevelStmt { $1 }
    | TSList TopLevelStmt { \gState -> $1 gState >>= $2 }

TopLevelStmt :: { GState -> Either Error GState }
TopLevelStmt
    : DoVarDeclare      
    {\gState -> do
        (_, gSyms') <- $1 gState (gsSyms gState)
        return $ gState { gsSyms = gSyms' }
    }
    | DoFuncDeclare     
    { \gState -> do
        let update = \funcs gState -> gState { gsFuncs = funcs }
        $1 (gsFuncs gState) update gState
        
    }
    | DoFuncDefine      
    { \gState -> do 
        let update = \funcs gState -> gState { gsFuncs = funcs }
        $1 (gsFuncs gState) update gState
    }

    | DoTypeDefine      
    { \gState -> do
        let update = \types gState -> gState { gsUserTypes = types }
        $1 (gsUserTypes gState) update gState
    }
    | TopLevelStmt ';'  {$1}

DoVarDeclare :: { forall a. SymTab a => GState -> a -> Either Error (Span, a) }
DoVarDeclare
    : Type1 IdentDims ';' 
    {\gState syms -> do
            type1 <- $1 gState
            let decls = map (\identDimSW ->
                                let (ident, dims) = spanWVal identDimSW
                                    span = getSpan identDimSW
                                in doVarDeclare ident (spanWVal type1) dims span) $2
            syms' <- (foldl1 (>=>) decls) syms
            return (getSpanBwn type1 $3, syms')
    }

IdentDims :: { [SpanW (String, [Int])] }
IdentDims
    : ident Dims        
    { [SpanW (spanWVal $1, spanWVal $2) (getSpanBwn $1 $2)] }
    | IdentDims ',' ident Dims
    { $1 ++ [SpanW (spanWVal $3, spanWVal $4) (getSpanBwn $3 $4)] }

Dims :: {SpanW [Int]}
Dims:
    {- empty -}         { SpanW ([]) (Span 0 0) }
  | Dims '[' number ']' { SpanW (spanWVal $1 ++ [spanWVal $3]) (getSpanBwn $1 $4) }

-- </DoVarDeclare>

-- <DoFuncDeclare>

DoFuncDeclare :: { [Func] -> ([Func] -> GState -> GState) -> GState -> Either Error GState }
DoFuncDeclare
    : Type1 ident '(' FunctionArgList ')' ';'
    { \funcs funcUpdate gState -> do
        type1 <- $1 gState
        argsList <- $4 gState
        let funcName = spanWVal $2
            argTypesSW = fmap (fmap snd) argsList
            declSpan = getSpanBwn type1 $6
        funcs' <- doFuncDeclare (spanWVal type1) funcName argTypesSW declSpan funcs
        return $ funcUpdate funcs' gState
    }

FunctionArg :: {GState -> Either Error (SpanW (String, Type1))}
FunctionArg
    : Type1 ident         
    { \gState -> do
        type1 <- $1 gState
        return $ SpanW (spanWVal $2, spanWVal type1) (getSpanBwn type1 $2) 
    }

FunctionArgList :: { GState -> Either Error [SpanW (String, Type1)] }
FunctionArgList
    : {- empty -} {\_ -> return []}
    | FunctionArgList_ {$1}

FunctionArgList_
    : FunctionArg
    { \gState -> do
        x <- $1 gState
        return [x]
    }
    | FunctionArgList_ ',' FunctionArg
    { \gState -> do
        xs <- $1 gState
        x <- $3 gState
        return $ xs ++ [x] 
    }
-- </DoFuncDeclare>

-- <DoFuncDefine>

DoFuncDefine :: { [Func] -> ([Func] -> GState -> GState) -> GState -> Either Error GState }
DoFuncDefine
    : Type1 ident '(' FunctionArgList ')' '{' FSlist '}'
    { \funcs funcUpdate gState -> do
        type1 <- $1 gState
        let funcName = spanWVal $2
        args <- $4 gState
        let declSpan = getSpanBwn type1 $5
        (funcs', funcDecl, lSyms, define) <- doFuncDefine
            (spanWVal type1)
            funcName
            args
            declSpan
            funcs
        let
            lState = LState
                        { lsFuncDecl  = funcDecl
                        , lsSyms      = lSyms
                        , lsLoopStack = loopStackInit
                        , lsIsTopLevel = True
                        , lsCurUserType = Nothing
                        }
            gState' = funcUpdate funcs' gState
        (lState', stmts) <- $7 lState gState'
        let funcs' = define (lsSyms lState') stmts
        return $ funcUpdate funcs' gState
    }

FSlist :: { LState -> GState -> Either Error (LState, [Stmt]) }
FSlist
    : FStmt              
    { \lState gState -> do
        (lState', stmt) <- $1 lState gState
        return (lState', maybeToList stmt)
    }
    | FSlist FStmt
    { \lState gState -> do
        (lState', stmts) <- $1 lState gState
        (lState'', stmt) <- $2 lState' gState
        return (lState'', stmts ++  (maybeToList stmt))
    }

FStmt :: { LState -> GState -> Either Error (LState, Maybe Stmt) }
FStmt
    : DoVarDeclare      
    { \lState gState -> do
        (span, lSyms') <- $1 gState (lsSyms lState)
        unless (lsIsTopLevel lState) 
            (Left $ Error.customError "Variable declaration not allowed here" span)
        return (lState { lsSyms = lSyms' }, Nothing)
    }
    | StmtIf            
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtIf stmt
    }
    | StmtIfElse        
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtIfElse stmt
    }
    | StmtWhile         
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtWhile stmt
    }
    | StmtReturn        
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtReturn stmt
    }
    | StmtAssign        
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtAssign stmt
    }
    | StmtRead          
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtRead stmt
    }
    | StmtWrite         
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtWrite stmt
    }
    | StmtRValue        
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtRValue stmt
    }
    | StmtInitialize    
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtInitialize stmt
    }
    | StmtAlloc         
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtAlloc stmt
    }
    | StmtNew
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtNew stmt
    }
    | StmtFree          
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtFree stmt
    }
    | StmtPoke          
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtPoke stmt
    }
    | StmtBreak         
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtBreak stmt
    }
    | StmtContinue      
    { \lState gState -> do
        stmt <- $1 lState gState
        return $ (lState, ) $ Just $ StmtContinue stmt
    }
    | FStmt ';'         { $1 }


StmtIf :: { LState -> GState -> Either Error StmtIf }
StmtIf
    : if '(' RValue ')' then FSlist endif             
    { \lState gState -> do
        cond <- $3 lState gState
        (_, thenSlist) <- $6 (lState{lsIsTopLevel = False}) gState
        mkStmtIf cond thenSlist
    }

StmtIfElse :: { LState -> GState -> Either Error StmtIfElse }
StmtIfElse
    : if '(' RValue ')' then FSlist else FSlist endif             
    { \lState gState -> do
        cond <- $3 lState gState
        (_, thenSlist) <- $6 lState{lsIsTopLevel = False} gState
        (_, elseSlist) <- $8 lState{lsIsTopLevel = False} gState
        mkStmtIfElse cond thenSlist elseSlist
    }

StmtWhile :: { LState -> GState -> Either Error StmtWhile }
StmtWhile
    : while '(' RValue ')' do FSlist endwhile
    { \lState gState -> do
        cond <- $3 lState gState
        (loopStack, define) <- mkStmtWhile cond (lsLoopStack lState)
        (_, fslist) <- $6 (lState{lsLoopStack = loopStack,  lsIsTopLevel = False}) gState
        return $ define fslist
    }

StmtAssign :: { LState -> GState -> Either Error StmtAssign }
StmtAssign
    : LValue '=' RValue ';'
    { \lState gState -> do
        lValue <- ($1 lState gState)
        rValue <- ($3 lState gState)
        mkStmtAssign (spanWVal lValue) (spanWVal rValue) (getSpanBwn lValue $4) 
    }

StmtRead :: { LState -> GState -> Either Error StmtRead }
StmtRead
    : read '(' LValue ')' ';'
    { \lState gState -> do
        lValue <- ($3 lState gState) 
        mkStmtRead lValue
    }

StmtWrite :: { LState -> GState -> Either Error StmtWrite }
StmtWrite:
    write '(' RValue ')' ';'
    { \lState gState -> do
        rValue <- ($3 lState gState)
        mkStmtWrite rValue
    }

StmtBreak :: { LState -> GState -> Either Error StmtBreak }
StmtBreak:
    break ';'         
    { \lState gState -> do
        let loopStack = lsLoopStack lState
        mkStmtBreak (getSpan $1) loopStack
    }

StmtContinue :: { LState -> GState -> Either Error StmtContinue }
StmtContinue:
    continue ';'      
    { \lState gState -> do
        let loopStack = lsLoopStack lState
        mkStmtContinue (getSpan $1) loopStack
    }

StmtRValue :: { LState -> GState -> Either Error StmtRValue }
StmtRValue:
    RValue ';'        
    { \lState gState -> do
        rValue <- ($1 lState gState)
        return $ mkStmtRValue (spanWVal rValue)
    }

StmtReturn :: { LState -> GState -> Either Error StmtReturn }
StmtReturn:
      return RValue ';' 
    { \lState gState -> do
        rValue <- $2 lState gState 
        mkStmtReturn (spanWVal rValue) (getSpanBwn $1 $3) (lsFuncDecl lState) 
    }

StmtInitialize :: { LState -> GState -> Either Error StmtInitialize }
StmtInitialize:
      initialize '(' ')' ';'
    { \lState gState -> do
        mkStmtInitialize (getSpanBwn $1 $4) 
    }

StmtAlloc :: { LState -> GState -> Either Error StmtAlloc }
StmtAlloc:
      LValue '=' alloc '(' ')' ';'
    { \lState gState -> do
        lValue <- $1 lState gState
        mkStmtAlloc (spanWVal lValue) (getSpanBwn lValue $5) 
    }

StmtNew :: { LState -> GState -> Either Error StmtNew }
StmtNew:
      LValue '=' new '(' Type1 ')' ';'
    { \lState gState -> do
        lValue <- $1 lState gState
        type1 <- $5 gState
        mkStmtNew (spanWVal lValue) (spanWVal type1) (getSpanBwn lValue $6) 
    }

StmtFree :: { LState -> GState -> Either Error StmtFree }
StmtFree:
      free '(' LValue ')' ';'
    { \lState gState -> do
        lValue <- $3 lState gState 
        mkStmtFree (spanWVal lValue) (getSpanBwn $1 $4) 
    }

StmtPoke :: { LState -> GState -> Either Error StmtPoke }
StmtPoke:
      poke '(' RValue ',' RValue ')' ';'
    { \lState gState -> do
        rValue1 <- ($3 lState gState)
        rValue2 <- ($5 lState gState)
        mkStmtPoke rValue1 rValue2 (getSpanBwn $1 $6) 
    }

LValue :: { LState -> GState -> Either Error (SpanW LValue) }
LValue
    : ident Indices     
    { \lState gState -> do
        let gSyms = gsSyms gState
            lSyms = lsSyms lState
        indices <- $2 lState gState
        mkLValueSymbol $1 indices lSyms gSyms
            <&> flip SpanW (getSpan $1) 
    }
    | LValue '.' ident Indices 
    { \lState gState -> do
        let userTypes = gsUserTypes gState
            ident = $3
        lValue <- $1 lState gState
        indices <- $4 lState gState
        mkLValueField (spanWVal lValue) ident indices userTypes
            <&> flip SpanW (getSpanBwn lValue (last indices)) 
    }

Indices :: { LState -> GState -> Either Error [SpanW RValue] }
Indices
    : {- empty -} {\_ _ -> return []}
    | Indices '[' RValue ']'
    { \lState gState -> do
        xs <- ($1 lState gState) 
        rValue <- $3 lState gState 
        return $ xs ++ [rValue] 
    }

RValue :: { LState -> GState -> Either Error (SpanW RValue) }
RValue
    : LValue            
    { \lState gState -> do
        lValue <- $1 lState gState 
        return $ RLValue `fmap` lValue
    }
    | Exp
    { \lState gState -> do 
        exp <- $1 lState gState
        return $ RExp `fmap` exp
    }
    | ident '(' RValues ')'
    { \lState gState -> do
        args <- $3 lState gState
        mkExpFuncCall (spanWVal $1) args (getSpanBwn $1 $4) (gsFuncs gState)
        <&> flip SpanW (getSpanBwn $1 $4) 
    }
    | LValue '.' ident '(' RValues ')'
    { \lState gState -> do
        lValue <- $1 lState gState
        args <- $5 lState gState
        mkExpMethodCall lValue (spanWVal $3) args (getSpanBwn lValue $6) (gsUserTypes gState) <&> flip SpanW (getSpanBwn lValue $6) 
    }
    | syscall '(' number ',' number ',' RValue ',' RValue ',' RValue ')' 
    { \lState gState -> do
        let intNum = $3
        let callNum = $5
        arg1 <- $7 lState gState
        arg2 <- $9 lState gState
        arg3 <- $11 lState gState
        return $ mkExpSyscall intNum callNum arg1 arg2 arg3 (getSpanBwn $1 $12)
                & flip SpanW (getSpanBwn $1 $12) 
    }
    | peek '(' RValue ')'
    { \lState gState -> do
        rValue <- ($3 lState gState)
        mkExpPeek rValue
        <&> flip SpanW (getSpanBwn $1 $4) 
    }
    | null
    { \_ _ -> return (SpanW RNull (getSpan $1))}

RValues :: { LState -> GState -> Either Error [SpanW RValue] }
RValues
    : {- empty -}  {\_ _ -> return []}
    | RValues_  {$1}
RValues_
    : RValue
    { \lState gState -> do
        x <- $1 lState gState
        return [x]
    }
    | RValues ',' RValue    
    { \lState gState -> do
        xs <- ($1 lState gState) 
        x <- $3 lState gState
        return $ xs ++ [x] 
    }

Exp :: { LState -> GState -> Either Error (SpanW Exp) }
Exp
    : number             { \_ _ -> return $ fmap ExpNum $1 }
    | '-' number         { \_ _ -> return $ fmap (\n -> ExpNum (-n)) $2 }
    | strlit             { \_ _ -> return $ fmap ExpStr $1 }
    | '(' Exp ')'        { $2 }
    | RValue '+' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpArithmetic r1 OpAdd r2) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '-' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpArithmetic r1 OpSub r2) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '*' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpArithmetic r1 OpMul r2) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '/' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpArithmetic r1 OpDiv r2 ) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '%' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpArithmetic r1 OpMod r2 ) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '<' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpRelational r1 OpLT r2 ) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '>' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpRelational r1 OpGT r2 ) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '<=' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpRelational r1 OpLE r2 ) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '>=' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpRelational r1 OpGE r2 ) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '==' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpRelational r1 OpEQ r2 ) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '!=' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpRelational r1 OpNE r2 ) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '&&' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpLogical r1 OpLAnd r2 ) <&> flip SpanW (getSpanBwn r1 r2) 
    }
    | RValue '||' RValue  
    { \lState gState -> do
        r1 <- $1 lState gState
        r2 <- $3 lState gState
        (mkExpLogical r1 OpLOr r2 ) <&> flip SpanW (getSpanBwn r1 r2) 
    }

Type1 :: { GState -> Either Error (SpanW Type1) }
Type1:
    ident
    { \gState -> do
        let userTypes = gsUserTypes gState
        mkType1 $1 userTypes
        <&> (flip SpanW (getSpan $1)) 
    } 

type_or_class
    : type { SpanW "type" (getSpan $1) }
    | class { SpanW "class" (getSpan $1) }

type_extends
    : {- empty -} {\_ -> return Nothing}
    | extends ident 
    {\gState -> do
        let name = spanWVal $2
        let nameSpan = getSpan $2
        let userTypes = gsUserTypes gState
        case (userTypeLookup name userTypes) of
            Just userType -> return $ Just userType
            Nothing -> Left $ Error.customError ("extends unknown type: " ++ name) 
                                                nameSpan
    }

DoTypeDefine :: { [UserType] -> ([UserType] -> GState -> GState) 
                  -> GState -> Either Error GState }
DoTypeDefine
    : type_or_class ident '{' TySList '}' type_extends
    { \userTypes userTypesUpdate gState -> do
        let name = spanWVal $2
            nameSpan = getSpan $2
        utParentType <- $6 gState
        case (userTypeLookup name userTypes) of
            Just userType ->
                Left $ errTypeRedeclared name (utDeclSpan userType) nameSpan
            Nothing -> return ()
         
        let userType = UserType { utName = name
                                , utFields = case utParentType of 
                                    Just ut -> utFields ut
                                    Nothing -> []
                                , utFuncs = []
                                , utDeclSpan = nameSpan
                                , utFieldsVisibility = (if ((spanWVal $1) == "type") 
                                    then SVPublic 
                                    else SVPrivate)
                                , utParentName = fmap utName utParentType
                                }
        let userTypes' = userTypeInsert userType userTypes
        let gState' = userTypesUpdate userTypes' gState
        let userTypeUpdate = \userType ->
                let userTypes' = userTypeInsert userType userTypes
                in userTypesUpdate userTypes'
        (gState', _)  <- $4 gState userType userTypeUpdate
        return gState'
    }


TyStmt :: {GState -> UserType 
           -> (UserType -> GState -> GState) -> Either Error (GState, UserType)}
TyStmt
    : DoVarDeclare 
    { \gState userType userTypeUpdate -> do
        (_, syms') <- $1 gState (utFields userType)
        let userType' = userType {utFields = syms'}
        return $ (userTypeUpdate userType' gState, userType')
    }
    | DoMethodDeclare
    { \gState userType userTypeUpdate -> do
        let funcs = utFuncs userType 
            funcUpdate = \funcs (gState, userType) -> 
                            let userType' = userType {utFuncs = funcs}
                                gState' = userTypeUpdate userType' gState
                            in (gState', userType')
        $1 funcs funcUpdate (gState, userType)
    }
    | DoMethodDefine
    { \gState userType userTypeUpdate -> do
        let funcs = utFuncs userType 
            funcUpdate = \funcs (gState, userType) -> 
                            let userType' = userType {utFuncs = funcs}
                                gState' = userTypeUpdate userType' gState
                            in (gState', userType')
        $1 funcs funcUpdate (gState, userType)
    }
    | TyStmt ';' {$1}

TySList :: {GState -> UserType 
           -> (UserType -> GState -> GState) -> Either Error (GState, UserType)}
TySList
    : TyStmt    {$1}
    | TySList TyStmt
    { \gState userType userTypeUpdate -> do
        (gState', userType') <- $1 gState userType userTypeUpdate
        $2 gState' userType' userTypeUpdate
    }

DoMethodDeclare :: { [Func] 
                  -> ([Func] -> (GState, UserType) -> (GState, UserType)) 
                  -> (GState, UserType) 
                  -> Either Error (GState, UserType) }
DoMethodDeclare
    : Type1 ident '(' FunctionArgList ')' ';'
    { \funcs funcUpdate (gState, userType) -> do
        type1 <- $1 gState
        argsList <- $4 gState
        let funcName = spanWVal $2
            argTypesSW = fmap (fmap snd) argsList
            declSpan = getSpanBwn type1 $6
        funcs' <- doFuncDeclare (spanWVal type1) funcName argTypesSW declSpan funcs
        return $ funcUpdate funcs' (gState, userType)
    }

DoMethodDefine :: {  [Func] 
                  -> ([Func] -> (GState, UserType) -> (GState, UserType)) 
                  -> (GState, UserType) 
                  -> Either Error (GState, UserType) }
DoMethodDefine
    : Type1 ident '(' FunctionArgList ')' '{' FSlist '}'
    { \funcs funcUpdate (gState, userType) -> do
        type1 <- $1 gState
        let funcName = spanWVal $2
        args <- $4 gState
        let declSpan = getSpanBwn type1 $5
        (funcs', funcDecl, lSyms, define) <- doFuncDefine
            (spanWVal type1)
            funcName
            args
            declSpan
            funcs
        let
            (gState', userType') = funcUpdate funcs' (gState, userType)
            lState = LState
                        { lsFuncDecl  = funcDecl
                        , lsSyms      = lSyms
                        , lsLoopStack = loopStackInit
                        , lsIsTopLevel = True
                        , lsCurUserType = Just userType'
                        }
        (lState', stmts) <- $7 lState gState'
        let funcs' = define (lsSyms lState') stmts
        return $ funcUpdate funcs' (gState', userType')
    }

{

-- Happy Error
parseError :: Token -> Frontend a
parseError token =
  throwError
    $ Error.customError ("Unexpected Token: " ++ (show token))
    $ getSpan token

}
