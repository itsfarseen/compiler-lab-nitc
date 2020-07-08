module Token where

import Span

data Token
  -- Arithmetic
  = TokenPlus Span
  | TokenMinus Span
  | TokenStar Span
  | TokenFwdSlash Span
  | TokenPercent Span
  -- Bool
  | TokenLT Span
  | TokenEQ Span
  | TokenNE Span
  | TokenGT Span
  | TokenLE Span
  | TokenGE Span
  -- Logical
  | TokenLAnd Span
  | TokenLOr Span
  -- Assignment
  | TokenEquals Span
  -- Punctuation
  | TokenSemiColon Span
  | TokenComma Span
  | TokenDot Span
  | TokenParOpen Span
  | TokenParClose Span
  | TokenBraceOpen Span
  | TokenBraceClose Span
  | TokenSqBracketOpen Span
  | TokenSqBracketClose Span
  -- Keywords
  | TokenRead Span
  | TokenWrite Span
  | TokenIf Span
  | TokenThen Span
  | TokenElse Span
  | TokenEndIf Span
  | TokenWhile Span
  | TokenEndWhile Span
  | TokenDo Span
  | TokenBreak Span
  | TokenContinue Span
  | TokenReturn Span
  | TokenType Span
  | TokenSyscall Span
  | TokenInitialize Span
  | TokenAlloc Span
  | TokenFree Span
  | TokenPeek Span
  | TokenPoke Span
  | TokenClass Span
  | TokenExtends Span
  -- Constants, Identifier
  | TokenNumber (SpanW Int)
  | TokenStrLit (SpanW String)
  | TokenIdent (SpanW String)
  -- EOF
  | TokenEOF Span
  deriving (Show)

instance HasSpan Token where
  getSpan token = case token of
    TokenPlus           span           -> span
    TokenMinus          span           -> span
    TokenStar           span           -> span
    TokenFwdSlash       span           -> span
    TokenPercent        span           -> span
    -- Bool
    TokenLT             span           -> span
    TokenEQ             span           -> span
    TokenNE             span           -> span
    TokenGT             span           -> span
    TokenLE             span           -> span
    TokenGE             span           -> span
    -- Logical
    TokenLAnd           span           -> span
    TokenLOr            span           -> span
    -- Assignment
    TokenEquals         span           -> span
    -- Punctuation
    TokenSemiColon      span           -> span
    TokenComma          span           -> span
    TokenDot            span           -> span
    TokenParOpen        span           -> span
    TokenParClose       span           -> span
    TokenBraceOpen      span           -> span
    TokenBraceClose     span           -> span
    TokenSqBracketOpen  span           -> span
    TokenSqBracketClose span           -> span
    -- Keywords
    TokenRead           span           -> span
    TokenWrite          span           -> span
    TokenIf             span           -> span
    TokenThen           span           -> span
    TokenElse           span           -> span
    TokenEndIf          span           -> span
    TokenWhile          span           -> span
    TokenEndWhile       span           -> span
    TokenDo             span           -> span
    TokenBreak          span           -> span
    TokenContinue       span           -> span
    TokenReturn         span           -> span
    TokenType           span           -> span
    TokenSyscall        span           -> span
    TokenInitialize     span           -> span
    TokenAlloc          span           -> span
    TokenFree           span           -> span
    TokenPeek           span           -> span
    TokenPoke           span           -> span
    TokenClass          span           -> span
    TokenExtends        span           -> span
    -- Constants, Identifier
    TokenNumber         (SpanW _ span) -> span
    TokenStrLit         (SpanW _ span) -> span
    TokenIdent          (SpanW _ span) -> span
    -- EOF
    TokenEOF            span           -> span
      -- Arithmetic
