module Definition
  ( Elem(Meta, Paragraph, Header, List, Code, Quote, Newlines)
  , Inline(Emph, Strong, Literal)
  , Inlines
  , ListCont(Numbered, Bulleted)
  , Level
  , Document
  ) where

data Elem
  = Meta String String
  | Paragraph Inlines
  | Header Level Inlines
  | List [ListCont]
  | Code (Maybe Language) Inlines
  | Quote [Elem]
  | Newlines
  deriving (Eq, Show)

data ListCont
  = Numbered Int Elem
  | Bulleted Elem
  deriving (Eq, Show)

data Inline
  = Emph Inlines
  | Strong Inlines
  | Literal String
  deriving (Eq, Show)

type Document = [Elem]

type Inlines = [Inline]

type Language = String

type Level = Int
