module Main (main) where

import Parser
import Writer
import Text.Parsec.String

main :: IO ()
main = do
  let file_ = "test.md"
  result <- parseFromFile pDocument file_
  case result of
    Left err -> print err
    Right md -> do
      let html = genHtml heading content
      writeFile "resource/gen.html" html
      putStrLn "Done !"
      where heading = head md
            content = tail md
