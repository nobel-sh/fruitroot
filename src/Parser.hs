module Parser(
        pDocument
        , pList
        , Elem(Paragraph, Header, List)
        , Inline(Emph, Strong, Literal)
        , Inlines
        , ListCont(Numbered, Bulleted)
        , Level
             )where

import Text.Parsec
import Text.Parsec.String

data Elem =
        Paragraph Inlines
        | Header Level Inlines
        | List [ListCont]
        deriving (Eq, Show)

data ListCont = Numbered Int Elem | Bulleted Elem
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
          , try pList
          , pParagraph
        ]

pDocument :: Parser Document
pDocument = manyTill pElem eof

pParagraph :: Parser Elem
pParagraph = do
  content <- manyTill pInline endOfLine
  return $ Paragraph content

pList :: Parser Elem
pList = do
          lists <- many1 pListCont
          return $ List lists

pListCont :: Parser ListCont
pListCont = try pNumberedList <|> try pBulletedList

pNumberedList :: Parser ListCont
pNumberedList = do
  idx <- many1 digit
  _   <- char '.'
  _   <- spaces
  content <- pParagraph
  return $ Numbered (read idx) content

pBulletedList :: Parser ListCont
pBulletedList = do
  _ <- oneOf "*-"
  _ <- spaces
  content <- pParagraph
  return $ Bulleted content


pHeading :: Parser Elem
pHeading = do
  level <- length <$> many1 (char '#')
  _ <- many1 space
  content <- manyTill pInline endOfLine
  return $ Header level content


pLiteral :: Parser Inline
pLiteral = do
  lit <- manyTill anyChar (lookAhead (oneOf "\n*-"))
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
