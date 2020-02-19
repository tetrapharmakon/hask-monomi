module Parenti where

import           Data.List
import           Data.List.Split

putParen :: String -> String
putParen s = "(" ++ s ++ ")"

parens :: [Integer] -> [[Integer]]
parens []  = []
parens [x] = [[x]]
parens xs  =
  let l = length xs
      cut i = splitAt i xs
   in [ map _this ((parens . snd) (cut i)) | i <- [1..l-1] ] ++ [[xs]]
