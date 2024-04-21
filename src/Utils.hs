module Utils(
    removeNewlines
) where

import Definition

removeNewlines = filter (\x -> x /= Newlines) 