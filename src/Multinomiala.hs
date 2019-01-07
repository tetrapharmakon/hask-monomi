module Multinomiala where

import           Data.List               hiding ( group )
import           Data.Either


type Monomial = (Integer, [(Char, Int)])
type Polynomial = [Monomial]

-- raggruppa elementi simili
pack :: (Eq a) => [a] -> [[a]]
pack []       = []
pack (x : xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

-- a few RLE utils
rleCompress :: Eq a => [a] -> [(a, Int)]
rleCompress [] = []
rleCompress y@(x : xs) =
 (x, length $ head $ pack y) : rleCompress (dropWhile (== x) xs)

rleReduce :: (Eq a, Ord a) => [(a, Int)] -> [(a, Int)]
rleReduce x = filter (\x -> snd x /= 0) $ map reducer $ groupBy (\x y -> fst x == fst y) $ sortOn fst x
 where reducer w = (fst $ head w, sum $ map snd w)

rleExpandPiece :: (a, Int) -> [a]
rleExpandPiece (a, n) = map (const a) [1 .. n]

rleExpand :: [(a, Int)] -> [a]
rleExpand = concatMap rleExpandPiece

rleAddToList :: (Eq a, Ord a) => [a] -> [(a, Int)] -> [(a, Int)]
rleAddToList ls x = rleReduce $ rleCompress (ls ++ rleExpand x)

-- reduction of a monomial
monoReduce :: Monomial -> Monomial
monoReduce m = (fst m, rleReduce (snd m))

-- algebra with monomials: multiplication
monoMultiply :: Monomial -> Monomial -> Monomial
monoMultiply p q = monoReduce (fst p * fst q, snd p ++ snd q)
-- isMultiple (tells if a monomial is scalar multiple of another), 
-- isLike (tells if a monomial has same indets, and possibly different coefficients)
-- monomial addition :: [monomial] -> polynomial

-- extract variables
monoExtractVar :: Monomial -> String
monoExtractVar m = map fst $ snd m
-- extract exponents
monoExtractExps :: Monomial -> [Int]
monoExtractExps m = map snd $ snd m

monoInit :: Integer -> String -> [Int] -> Monomial
monoInit coef vees exps = (coef, zip vees exps)

-- show polynomial
monoJoin :: (Char,Int) -> String
monoJoin (x,e) = [x] ++ "^" ++ show e

monoShow :: Monomial -> String
monoShow m = show (fst m) ++ concatMap monoJoin (snd m)
