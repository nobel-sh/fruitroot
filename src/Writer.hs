{-# LANGUAGE QuasiQuotes #-}

module Writer
  ( makeTitle
  , genHtml
  , writeList
  , htmlBlock
  , makeNavbar
  ) where

import           Data.List         (intercalate)
import           Definition
import           Parser
import           Text.RawString.QQ

genHtml metadata content =
  "<html>\n" <>
  htmlHead metadata <>
  "\n<body>\n" <>
  makeNavbar <>
  makeHeader metadata content <>
  "\n<main> <article>\n" <>
  htmlBody (tail content) <> "\n</article> </main>\n" <> "\n</body>\n</html>"

makeHeader (Meta _ author) content =
  "<header>\n" <>
  htmlBlock content' -- renders title
   <>
  htmlBlock (Header 3 author') -- renders author
   <>
  "</header>"
  where
    author' = [Literal $ "Author: " <> author]
    content' =
      case head content of
        Newlines -> head $ tail content
        _        -> head content
makeHeader _ _ = ""

headStatic =
  [r|
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Martian+Mono:wght@100..800&display=swap" rel="stylesheet">
    <link rel="stylesheet" href="styles.css">
|]

htmlHead (Meta title _) =
  "<head>" <> headStatic <> "<title>" <> title <> "</title>" <> "</head>"

htmlBody []     = ""
htmlBody (x:xs) = htmlBlock x <> "\n" <> htmlBody xs

makeTitle (Header 1 content) =
  "<header>" <> htmlBlock (Header 1 content) <> "</header>" <> "\n"
makeTitle _ = "" -- HACK: removes empty paragraphs that are on upon empty \n

makeNavbar = "<div class=\"nav\">"
            <> "<a href=\"#\" >Button 1</a>"
            <> "<a href=\"#\" >Button 2</a>"
            <> "<a href=\"#\" >Button 3</a>"
            <> "<a href=\"#\" >Button 4</a>"
            <> "</div>"

makeAuthor (Meta _ author) = "<h3>" <> "Author: " <> author <> "</h3>" <> "\n"

htmlBlock Newlines = "" -- HACK: Skip newlines
htmlBlock (Paragraph content) = "<span>" <> htmlInlines content <> "</span>"
-- TODO: Maybe add sytax highlighthing for popular langs?
htmlBlock (Code _ content) =
  "<pre> <code>" <> htmlInlines content <> "</code> </pre>"
htmlBlock (Quote contents) =
  "<blockquote>\n" <>
  intercalate "\n" (map writeQuote contents) <> "\n</blockquote>"
  where
    writeQuote (Paragraph c) = htmlInlines c <> "<br>"
htmlBlock (Header l content) =
  "<h" <> show l <> ">" <> htmlInlines content <> "</h" <> show l <> ">"
htmlBlock (List contents) =
  case head contents of
    (Numbered _ _) -> "<ol>\n" <> writeListCont contents <> "</ol>"
    (Bulleted _)   -> "<ul>\n" <> writeListCont contents <> "</ul>"

writeListCont []     = ""
writeListCont (x:xs) = "<li>" <> writeList x <> "</li>\n" <> writeListCont xs

writeList (Numbered _ content) = htmlBlock content
writeList (Bulleted content)   = htmlBlock content

htmlInlines [] = ""
htmlInlines xs = foldr ((<>) . htmlInline) "" xs

htmlInline (Literal content) = content
htmlInline (Emph content)    = "<em>" <> htmlInlines content <> "</em>"
htmlInline (Strong content)  = "<strong>" <> htmlInlines content <> "</strong>"
