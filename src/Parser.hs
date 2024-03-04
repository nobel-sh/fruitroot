module Parser(
        pDocument
        , pQuote
        , Elem(Paragraph, Header, List, Code, Quote)
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
        | Code (Maybe Language) Inlines
        | Quote [Elem]
        deriving (Eq, Show)

data ListCont = Numbered Int Elem | Bulleted Elem
        deriving (Eq, Show)

data  Inline =
        Emph Inlines
        | Strong Inlines
        | Literal String
        deriving (Eq, Show)

type Inlines = [Inline]

type Language = String
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
          , try pCode
          , try pQuoteBlock
          , pParagraph
        ]

pDocument :: Parser Document
pDocument = manyTill pElem eof

pQuoteBlock :: Parser Elem
pQuoteBlock = do
  contents <- many1 pQuote
  return $ Quote contents

pQuote :: Parser Elem
pQuote = do
  char '>' *> spaces
  content <- pParagraph
  return content

pCode :: Parser Elem
pCode = do
  _ <- string "```"
  lang <- optionMaybe (try (char ' ' *> many1 (noneOf "\n")))
  _ <- endOfLine
  content <- manyTill anyChar (string "\n```" <* endOfLine)
  return $ Code lang [(Literal content)]

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
