{-# LANGUAGE QuasiQuotes #-}

module Writer(
  makeTitle
  , genHtml
  , writeList
  , htmlBlock
             ) where

import Text.RawString.QQ
import Parser

genHtml heading content = 
            "<html>\n"
            <> htmlHead
            <> "\n<body>"
            <> makeTitle heading
            <> "\n<main> <article>\n"
            <> htmlBody content
            <> "\n</article> </main>\n"
            <> "\n</body>\n</html>"

htmlHead = [r|<head>
    <meta charset="UTF-8">
    <meta name="viewport" content="width=device-width, initial-scale=1.0">
    <title>My Blog Post</title>
    <link rel="preconnect" href="https://fonts.googleapis.com">
    <link rel="preconnect" href="https://fonts.gstatic.com" crossorigin>
    <link href="https://fonts.googleapis.com/css2?family=Martian+Mono:wght@100..800&display=swap" rel="stylesheet">
    <link rel="stylesheet" href="styles.css">
</head>
|]

htmlBody [] = ""
htmlBody (x:xs) = htmlBlock x <> "\n" <> htmlBody xs

makeTitle :: Elem -> String
makeTitle (Header 1  content) = "<header>"
                        <> htmlBlock (Header 1 content)
                        <> "</header>"

htmlBlock (Paragraph content) = "<p>" <> htmlInlines content<> "</p>"
htmlBlock (Code content) = "<code>" <> htmlInlines content<> "</code>"
htmlBlock (Header l content)  = "<h"<>show l<>">"
                          <> htmlInlines content
                          <> "</h"<>show l <>">"

htmlBlock (List contents)  =
            case head contents of
              (Numbered _  _) -> "<ol>\n"
                                 <> writeListCont contents
                                 <> "</ol>"
              (Bulleted _)    -> "<ul>\n"
                                 <> writeListCont contents
                                 <> "</ul>"


writeListCont [] = ""
writeListCont (x:xs) = "<li>"
                       <> writeList x
                       <> "</li>\n"
                       <> writeListCont xs

writeList (Numbered _ content) = htmlBlock content
writeList (Bulleted content)   = htmlBlock content

htmlInlines [] = ""
htmlInlines (x:xs) = htmlInline x <> htmlInlines xs

htmlInline (Literal content) = content
htmlInline (Emph    content) = "<em>"
                        <> htmlInlines content
                        <> "</em>"
htmlInline (Strong  content) = "<strong>"
                        <> htmlInlines content
                        <> "</strong>"
