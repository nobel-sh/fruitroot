module Main (main) where

import Parser
import Definition
import Writer
import Text.Parsec.String
import Utils

main :: IO ()
main = do
  let file_ = "test.md"
  result <- parseFromFile pDocument file_
  case result of
    Left err -> print err
    Right md -> do
      putStrLn $ show md
      putStrLn $ show content
      let html = genHtml metadata content
      writeFile "resource/gen.html" html
      putStrLn "Done !"
      where metadata = head md
            content  = removeNewlines $ tail md
