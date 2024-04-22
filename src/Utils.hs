module Utils
  ( removeNewlines
  ) where

import           Definition

removeNewlines = filter (/= Newlines)
