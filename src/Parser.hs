module Parser(
        pDocument
        , Elem(Paragraph,Header)
        , Inline(Emph,Strong,Literal)
        , Inlines
        , Level
             )where

import Text.Parsec
import Text.Parsec.String

data Elem =
        Paragraph Inlines
        | Header Level Inlines
        deriving (Eq, Show)

data  Inline =
        Emph Inlines
        | Strong Inlines
        | Literal String
        deriving (Eq, Show)

type Inlines = [Inline]

type Document = [Elem]
type Level = Int

pInline :: Parser Inline
pInline = choice[
            try pStrong
            , try pEmph
            , pLiteral]

pElem :: Parser Elem
pElem = choice [
          try pHeading
          , pParagraph
        ]

pDocument :: Parser Document
pDocument = manyTill pElem eof

pParagraph :: Parser Elem
pParagraph = do
  content <- manyTill pInline endOfLine
  return $ Paragraph content

pHeading :: Parser Elem
pHeading = do
  level <- length <$> many1 (char '#')
  _ <- many1 space
  content <- manyTill pInline endOfLine
  return $ Header level content


pLiteral :: Parser Inline
pLiteral = do
  lit <- manyTill anyChar (lookAhead (oneOf "\n*_"))
  return $ Literal lit

pStrong :: Parser Inline
pStrong = do
  _ <- string "**"
  content <- manyTill pInline (string "**")
  return $ Strong content

pEmph :: Parser Inline
pEmph = do
  _ <- string "*"
  content <- manyTill pInline (string "*")
  return $ Emph content
