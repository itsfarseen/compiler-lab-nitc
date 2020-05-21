{
module Parser where

import qualified Lexer
import Frontend
import Grammar
import Token

import Span
import Error (Error)
import qualified Error
import Symbol (DataType(..))

import Text.Printf
import Control.Monad.Error

}

%name parse
%monad {Frontend}
%lexer {Lexer.lexer} {TokenEOF _}
%tokentype {Token}
%error {parseError}

%token
    number   {TokenNumber    $$}
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

    ';'      {TokenSemiColon _ }
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

Program:
      begin Slist end       {Program {stmts=$2}}
    | begin end             {Program {stmts=[]}}

Slist:
      Slist Stmt            {$1 ++ [$2]}
    | Stmt                  {[$1]}

Stmt:
      StmtDeclare           {StmtDeclare $1}
    | StmtAssign            {StmtAssign $1}
    | StmtRead              {StmtRead $1}
    | StmtWrite             {StmtWrite $1}
    | StmtIf                {StmtIf $1}
    | StmtIfElse            {StmtIfElse $1}
    | StmtWhile             {StmtWhile $1}
    | StmtDoWhile           {StmtDoWhile $1}
    | StmtBreak             {StmtBreak $1}
    | StmtContinue          {StmtContinue $1}
    | Stmt ';'              {$1}

StmtDeclare:
    DataType ident ';'     {% mkStmtDeclare (spanWVal $2) (spanWVal $1) (getSpanBwn $1 $3)}

StmtAssign:
    Ident '=' Exp ';'     {% mkStmtAssign $1 $3 (getSpanBwn $1 $4)}

StmtRead:
    read '(' Ident ')' ';' 
                          {% mkStmtRead $3 (getSpanBwn $1 $5)}

StmtWrite:
    write '(' Exp ')' ';' {% mkStmtWrite $3 (getSpanBwn $1 $5)}

StmtIf:
      if '(' Exp ')' 
      then 
        Slist 
      endif
                          {% mkStmtIf $3 $6 (getSpanBwn $1 $7)}

    | if '(' Exp ')' '{' 
        Slist 
     '}' 
                          {% mkStmtIf $3 $6 (getSpanBwn $1 $7)}
                        
StmtIfElse:
      if '(' Exp ')' then 
        Slist 
      else 
        Slist 
      endif 
                          {% mkStmtIfElse $3 $6 $8 (getSpanBwn $1 $9)}

    | if '(' Exp ')' '{' 
        Slist 
      '}' else '{' 
        Slist 
      '}' 
                          {% mkStmtIfElse $3 $6 $10 (getSpanBwn $1 $11)}

StmtWhileEnter1:
      while '(' Exp ')' do {% pushLoopStack >> return $3 }

StmtWhileExit1:
      endwhile             {% popLoopStack >> return $1 }

StmtWhileEnter2:
      while '(' Exp ')' '{' {% pushLoopStack >> return $3 }

StmtWhileExit2:
      '}'                   {% popLoopStack >> return $1 }

StmtWhile:
      StmtWhileEnter1
        Slist 
      StmtWhileExit1 
                          {% mkStmtWhile $1 $2 (getSpanBwn $1 $3)}

    | StmtWhileEnter2
        Slist 
      StmtWhileExit2
                          {% mkStmtWhile $1 $2 (getSpanBwn $1 $3)}

StmtDoWhileEnter:
      do '{'              {% pushLoopStack >> return $1 }

StmtDoWhileExit:
      '}' while '(' Exp ')' {% popLoopStack >> return $4 }

StmtDoWhile:
      StmtDoWhileEnter
        Slist 
      StmtDoWhileExit
                          {% mkStmtDoWhile $3 $2 (getSpanBwn $1 $3)}

StmtBreak:
      break ';'           {% mkStmtBreak (getSpanBwn $1 $2)}

StmtContinue:
      continue ';'        {% mkStmtContinue (getSpanBwn $1 $2)}

Ident:
     ident                {% mkIdent (spanWVal $1) (getSpan $1)}

Exp: 
     number               {ExpPure $ ExpNum (spanWVal $1) (getSpan $1)}
   | Ident                {ExpIdent $ MkExpIdent $1}
   | '(' Exp ')'          {$2}
   | Exp '+' Exp          {ExpPure $ ExpArithmetic $1 OpAdd $3 (getSpanBwn $1 $3)}
   | Exp '-' Exp          {ExpPure $ ExpArithmetic $1 OpSub $3 (getSpanBwn $1 $3)}
   | Exp '*' Exp          {ExpPure $ ExpArithmetic $1 OpMul $3 (getSpanBwn $1 $3)}
   | Exp '/' Exp          {ExpPure $ ExpArithmetic $1 OpDiv $3 (getSpanBwn $1 $3)}
   | Exp '%' Exp          {ExpPure $ ExpArithmetic $1 OpMod $3 (getSpanBwn $1 $3)}
   | Exp '<' Exp          {ExpPure $ ExpLogical $1 OpLT  $3 (getSpanBwn $1 $3)}
   | Exp '>' Exp          {ExpPure $ ExpLogical $1 OpGT  $3 (getSpanBwn $1 $3)}
   | Exp '<=' Exp         {ExpPure $ ExpLogical $1 OpLE  $3 (getSpanBwn $1 $3)}
   | Exp '>=' Exp         {ExpPure $ ExpLogical $1 OpGE  $3 (getSpanBwn $1 $3)}
   | Exp '==' Exp         {ExpPure $ ExpLogical $1 OpEQ  $3 (getSpanBwn $1 $3)}
   | Exp '!=' Exp         {ExpPure $ ExpLogical $1 OpNE  $3 (getSpanBwn $1 $3)}

DataType:
    int                   {SpanW DataTypeInt (getSpan $1)}
  | bool                  {SpanW DataTypeBool (getSpan $1)}


{

parseError :: Token -> Frontend a
parseError token = throwError $ Error.syntaxError $ getSpan token

}