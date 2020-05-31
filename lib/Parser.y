{
{-# LANGUAGE FlexibleContexts #-}
module Parser where

import qualified Lexer
import Frontend
import Grammar
import Token
import Span
import Error (Error)
import qualified Error

import Text.Printf
import Control.Monad.Error
import Data.Functor

}

%name parse
%monad {Frontend}
%lexer {Lexer.lexer} {TokenEOF _}
%tokentype {Token}
%error {parseError}

%token
    number   {TokenNumber    $$}
    strlit   {TokenStrLit    $$}
    ident    {TokenIdent     $$}
 
    '+'      {TokenPlus      _ }
    '-'      {TokenMinus     _ }
    '*'      {TokenStar      _ }
    '/'      {TokenFwdSlash  _ }
    '%'      {TokenPercent   _ }

    '('      {TokenParOpen   _ }
    ')'      {TokenParClose  _ }

    '{'      {TokenBraceOpen _ }
    '}'      {TokenBraceClose _}

    '['      {TokenSqBracketOpen _ }
    ']'      {TokenSqBracketClose _}

    ';'      {TokenSemiColon _ }
    ','      {TokenComma     _ }
    '='      {TokenEquals    _ }

    '<'      {TokenLT        _ }
    '=='     {TokenEQ        _ }
    '!='     {TokenNE        _ }
    '>'      {TokenGT        _ }
    '<='     {TokenLE        _ }
    '>='     {TokenGE        _ }

    begin    {TokenBegin     _ }
    end      {TokenEnd       _ }

    read     {TokenRead      _ }
    write    {TokenWrite     _ }

    if       {TokenIf        _ }
    then     {TokenThen      _ }
    else     {TokenElse      _ }
    endif    {TokenEndIf     _ }
    while    {TokenWhile     _ }
    endwhile {TokenEndWhile  _ }
    do       {TokenDo        _ }
    break    {TokenBreak     _ }
    continue {TokenContinue  _ }
    int      {TokenInt       _ }
    bool     {TokenBool      _ }
    string   {TokenString    _ }

%nonassoc '=' '==' '<' '>' '<=' '>=' '!='
%left '+' '-'
%left '*' '/' '%'

-- Intermixing of `if (..) then ..` and `if ( .. ) { .. }` are disallowed.
-- This can lead to ambiguities: `if (..) then if ( .. ) { .. } else .. endif`
%nonassoc '{' '}'
%nonassoc else

%%

Program :: {Program}
Program:
      begin Slist end       {Program {stmts=$2}}
    | begin end             {Program {stmts=[]}}

Slist :: {[Stmt]}
Slist:
      Slist Stmt            {$1 ++ $2}
    | Stmt                  {$1}

Stmt :: {[Stmt]}
Stmt:
      StmtDeclare           {fmap StmtDeclare $1}
    | StmtAssign            {[StmtAssign $1]}
    | StmtRead              {[StmtRead $1]}
    | StmtWrite             {[StmtWrite $1]}
    | StmtIf                {[StmtIf $1]}
    | StmtIfElse            {[StmtIfElse $1]}
    | StmtWhile             {[StmtWhile $1]}
    | StmtDoWhile           {[StmtDoWhile $1]}
    | StmtBreak             {[StmtBreak $1]}
    | StmtContinue          {[StmtContinue $1]}
    | Stmt ';'              {$1}

StmtDeclare :: {[StmtDeclare]}
StmtDeclare:
      PrimitiveType Dims IdentDims ';'   {% mapM (\x -> mkStmtDeclare (spanWVal (fst x)) (spanWVal $1) ((spanWVal $2 )++ (spanWVal (snd x))) (getSpanBwn $1 $4)) $3}

IdentDims :: {[(SpanW String, SpanW [Int])]}
IdentDims:
      ident Dims                      {[($1, $2)]}
    | IdentDims ',' ident Dims        {$1 ++ [($3, $4)]}

StmtAssign :: {StmtAssign}
StmtAssign:
    LValue '=' RValue ';'    {% mkStmtAssign (spanWVal $1) (spanWVal $3) (getSpanBwn $1 $3)}

StmtRead :: {StmtRead}
StmtRead:
    read '(' LValue ')' ';'  {% mkStmtRead $3 }

StmtWrite :: {StmtWrite}
StmtWrite:
    write '(' RValue ')' ';' {% mkStmtWrite $3 }

StmtIf :: {StmtIf}
StmtIf:
      if '(' RValue ')' 
      then 
        Slist 
      endif                {% mkStmtIf $3 $6 }

    | if '(' RValue ')' '{' 
        Slist 
     '}'                   {% mkStmtIf $3 $6 }
                        
StmtIfElse :: {StmtIfElse}
StmtIfElse:
      if '(' RValue ')' then 
        Slist 
      else 
        Slist 
      endif                {% mkStmtIfElse $3 $6 $8 }

    | if '(' RValue ')' '{' 
        Slist 
      '}' else '{' 
        Slist 
      '}'                  {% mkStmtIfElse $3 $6 $10 }

StmtWhileEnter1 :: {SpanW RValue}
StmtWhileEnter1:
      while '(' RValue ')' do {% pushLoopStack >> return $3 }

StmtWhileExit1 :: {Token}
StmtWhileExit1:
      endwhile             {% popLoopStack >> return $1 }

StmtWhileEnter2 :: {SpanW RValue}
StmtWhileEnter2:
      while '(' RValue ')' '{' {% pushLoopStack >> return $3 }

StmtWhileExit2 :: {Token}
StmtWhileExit2:
      '}'                   {% popLoopStack >> return $1 }

StmtWhile :: {StmtWhile}
StmtWhile:
      StmtWhileEnter1
        Slist 
      StmtWhileExit1      {% mkStmtWhile $1 $2 }

    | StmtWhileEnter2
        Slist 
      StmtWhileExit2      {% mkStmtWhile $1 $2 }

StmtDoWhileEnter :: {Token}
StmtDoWhileEnter:
      do '{'              {% pushLoopStack >> return $1 }

StmtDoWhileExit :: {SpanW RValue}
StmtDoWhileExit:
      '}' while '(' RValue ')' {% popLoopStack >> return $4 }

StmtDoWhile :: {StmtDoWhile}
StmtDoWhile:
      StmtDoWhileEnter
        Slist 
      StmtDoWhileExit     {% mkStmtDoWhile $3 $2 }

StmtBreak :: {StmtBreak}
StmtBreak:
      break ';'           {% mkStmtBreak (getSpan $1)}

StmtContinue :: {StmtContinue}
StmtContinue:
      continue ';'        {% mkStmtContinue (getSpan $1)}

LValue :: {SpanW LValue}
LValue:
      Ident               {% mkLValue $1 [] <&> flip SpanW (getSpan $1)}
    | Ident Indices       {% mkLValue $1 (spanWVal $2) <&> flip SpanW (getSpanBwn $1 $2)}
    
Indices :: {SpanW [SpanW RValue]}
Indices:
      '[' RValue ']'          {SpanW ([$2]) (getSpanBwn $1 $3)}
    | Indices '[' RValue ']'  {SpanW (spanWVal $1 ++ [$3]) (getSpanBwn $1 $4)}

RValue :: {SpanW RValue}
RValue:
      LValue             {fmap RLValue $1}
    | Exp                {fmap RExp $1}

Ident :: {SpanW Ident}
Ident:
     ident                {% (mkIdent $1) <&> flip SpanW (getSpan $1)}

Exp :: {SpanW Exp}
Exp: 
     number               {fmap ExpNum $1}
   | strlit               {fmap ExpStr $1}
   | '(' Exp ')'          {$2}
   | RValue '+' RValue    {% (mkExpArithmetic $1 OpAdd $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '-' RValue    {% (mkExpArithmetic $1 OpSub $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '*' RValue    {% (mkExpArithmetic $1 OpMul $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '/' RValue    {% (mkExpArithmetic $1 OpDiv $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '%' RValue    {% (mkExpArithmetic $1 OpMod $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '<' RValue    {% (mkExpLogical $1 OpLT  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '>' RValue    {% (mkExpLogical $1 OpGT  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '<=' RValue   {% (mkExpLogical $1 OpLE  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '>=' RValue   {% (mkExpLogical $1 OpGE  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '==' RValue   {% (mkExpLogical $1 OpEQ  $3) <&> flip SpanW (getSpanBwn $1 $3)}
   | RValue '!=' RValue   {% (mkExpLogical $1 OpNE  $3) <&> flip SpanW (getSpanBwn $1 $3)}

PrimitiveType :: {SpanW PrimitiveType}
PrimitiveType:
    int                     {SpanW TypeInt (getSpan $1)}
  | bool                    {SpanW TypeBool (getSpan $1)}
  | string                  {SpanW TypeString (getSpan $1)}

Dims :: {SpanW [Int]}
Dims:
    {- empty -}             {SpanW ([]) (Span 0 0)}
  | Dims '[' number ']'     {SpanW (spanWVal $1 ++ [spanWVal $3]) (getSpanBwn $1 $4)}

{

parseError :: Token -> Frontend a
parseError token = throwError $ Error.customError ("Parsing error 2: " ++ (show token)) $ getSpan token

}