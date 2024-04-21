module Utils(
    removeNewlines
) where

import Parser

removeNewlines = filter (\x -> x /= Newlines) 