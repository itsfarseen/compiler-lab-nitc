module Span where

data Span =
  Span
    { spanStart :: Int
    , spanLength :: Int
    }
  deriving (Show)

(<->) :: Span -> Span -> Span
(<->) span1 span2 =
  let start  = spanStart span1
      end    = spanStart span2 + spanLength span2
      length = end - start + 1
  in  Span start length

data SpanW a =
  SpanW a Span
  deriving (Show)

instance Functor SpanW where
  fmap f (SpanW a span) = SpanW (f a) span

spanWVal (SpanW a _) = a

class HasSpan a where
  getSpan :: a -> Span

instance HasSpan (SpanW a) where
  getSpan (SpanW _ span) = span

getSpanBwn a b = getSpan a <-> getSpan b

data FullSpan =
  FullSpan
    { fspanLineNo :: Int
    , fspanColNo :: Int
    , fspanLine :: String
    , fspanLength :: Int
    }

getFullSpan :: Span -> String -> FullSpan
getFullSpan span sourceCode =
  let linesBefore = lines $ take (spanStart span) sourceCode
      lenLastLine = length (last linesBefore)
      lineNo      = length linesBefore
      colNo       = lenLastLine + 1
      lineStart   = spanStart span - colNo + 1
      line        = head (lines $ drop lineStart sourceCode)
  in  FullSpan { fspanLineNo = lineNo
               , fspanColNo  = colNo
               , fspanLine   = line
               , fspanLength = spanLength span
               }
