module Parenti where

import Data.List
import Data.List.Split

putParen :: String -> String
putParen s = "(" ++ s ++ ")"

-- parens :: String -> [String]
-- parens "" = []
-- parens [x] = [putParen [x]]
-- parens [x, y] = [putParen [x, y], putParen [x] ++ putParen [y]]
parens s =
  let ell = length s
      fun n s = (putParen (fs n), sn n)
   in [fun n s | n <- [1 .. (ell - 1)]]
  where
    fs n = fst $ splitAt n s
    sn n = snd $ splitAt n s -- let that i = putParen (take i s)
  --     this i = parens (drop i s)
  --  in (concat . concat)
  --       [[map ([x] ++) (this i) | x <- that i] | i <- [1 .. (length s - 1)]]

applyPiece n s = (putParen (fst $ splitAt n s) ++)

dat n s = parens $ snd $ splitAt n s
