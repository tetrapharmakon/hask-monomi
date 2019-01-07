module Multinomiala where

import           Data.List
import           Data.Either

-- TODO:
-- fix monoSum
-- QuickCheck tests:
-- 1) monoInit (fst p) (monoExtractVar p) (monoExtractExps p) == p
-- 2) monoSum is associative


type Monomial = (Integer, [(Char, Int)])
type Polynomial = [Monomial]

-- groups similar elements
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

-- isMultiple (tells if a monomial is scalar multiple of another;
-- will be a useful sort key to reduce polynomials), 
isMultiple :: Monomial -> Monomial -> Bool 
isMultiple m n = sort (snd m) == sort (snd n)
-- isLike (tells if a monomial has same indets, and possibly different exponents;
-- not sure if this is needed?)
isLike :: Monomial -> Monomial -> Bool 
isLike m n = sort (monoExtractVar m) == sort (monoExtractVar n)

-- extract variables
monoExtractVar :: Monomial -> String
monoExtractVar m = map fst $ snd m

-- extract exponents
monoExtractExps :: Monomial -> [Int]
monoExtractExps m = map snd $ snd m

-- Initialize a monomial
monoInit :: Integer -> String -> [Int] -> Monomial
monoInit coef vees exps = (coef, zip vees exps)

-- show polynomial
monoJoin :: (Char,Int) -> String
monoJoin (x,e) | e == 0 = ""
               | e == 1 = [x]
               | e > 1  = [x] ++ "^" ++ show e

monoShow :: Monomial -> String
monoShow m | fst m == 1 = concatMap monoJoin (snd m)
           | otherwise = show (fst m) ++ concatMap monoJoin (snd m)

-- raise a monomial to the exp-power 
monoRaise :: Monomial -> Int -> Monomial 
monoRaise m exp = (fst m ^ exp, map (raiser exp) (snd m))
  where
    raiser n z = (fst z,n * snd z)

-- fix it:
-- monoSum :: Monomial -> Monomial -> Either Polynomial Monomial
-- monoSum m n | isMultiple m n = Right (fst m + fst n,snd m)
--             | otherwise      = Left [m,n]
-- is more elegant.
-- Currently, monoSum _assumes_ that its arguments are summable

-- sum a Monomial
monoSum' :: Monomial -> Monomial -> Monomial
monoSum' m n = (fst m + fst n,snd m)

monoSum :: [Monomial] -> Monomial
monoSum [] = (0, [])
monoSum ms = (sum $ map fst ms, snd $ head ms)

-- reduce a polynomial
polyReduce :: Polynomial -> Polynomial
polyReduce p = filter (\x -> snd x /= []) $ map monoSum $ groupBy (\x y -> isMultiple x y) $ sortOn snd p

-- sum two polynomials
polySum' :: Polynomial -> Polynomial -> Polynomial
polySum' p q = polyReduce (p ++ q)

polySum :: [Polynomial] -> Polynomial
polySum ps = polyReduce $ foldl polySum' [(0,[])] ps

-- show a polynomial
polyShow :: Polynomial -> String
polyShow p = intercalate " + " $ map monoShow $ polyReduce p

a = monoInit 3 "x" [0]
a' = monoInit 3 "y" [0]
b = monoInit 1 "x" [1]
c = monoInit 1 "y" [1]

aa' = [a,a']
bc = [b,c]