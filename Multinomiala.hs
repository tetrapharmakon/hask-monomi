module Multinomiala where

import Data.Either ()
import Data.List
import Data.Maybe ()

type Monomial = (Integer, [(Char, Int)])

type Polynomial = [Monomial]

-- groups similar elements
pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack (x:xs) = (x : takeWhile (== x) xs) : pack (dropWhile (== x) xs)

-- a few RLE utils
rleCompress :: Eq a => [a] -> [(a, Int)]
rleCompress [] = []
rleCompress y@(x:xs) =
  (x, length $ head $ pack y) : rleCompress (dropWhile (== x) xs)

rleReduce :: (Eq a, Ord a) => [(a, Int)] -> [(a, Int)]
rleReduce [] = []
rleReduce a = filter appears $ map reducer $ groupBy alike $ sortOn fst a
  where
    appears x = snd x /= 0
    alike x y = fst x == fst y
    reducer w = (fst $ head w, sum $ map snd w)

rleExpandPiece :: (a, Int) -> [a]
rleExpandPiece (a, n) = map (const a) [1 .. n]

rleExpand :: [(a, Int)] -> [a]
rleExpand = concatMap rleExpandPiece

rleAddToList :: (Eq a, Ord a) => [a] -> [(a, Int)] -> [(a, Int)]
rleAddToList ls x = rleReduce $ rleCompress (ls ++ rleExpand x)

-- reduction of a monomial
monoReduce :: Monomial -> Monomial
monoReduce m
  | fst m == 0 = (0, [])
  | otherwise = (fst m, rleReduce (snd m))

-- algebra with monomials: multiplication
monoMultiply :: Monomial -> Monomial -> Monomial
monoMultiply p q = monoReduce (fst p * fst q, snd p ++ snd q)

-- isMultiple (tells if a monomial is scalar multiple of another;
-- will be a useful sort key to reduce polynomials), 
isMultiple :: Monomial -> Monomial -> Bool
isMultiple m n = sort (snd m) == sort (snd n)

-- extract variables
monoExtractVar :: Monomial -> String
monoExtractVar m = map fst $ snd m

-- extract exponents
monoExtractExps :: Monomial -> [Int]
monoExtractExps m = map snd $ snd m

-- Initialize a monomial
monoInit :: Integer -> String -> [Int] -> Monomial
monoInit coef vees exps = monoReduce (coef, zip vees exps)

-- auxiliary initialization of const mo/poly-nomial
monoConst :: Integer -> Monomial
monoConst n = monoInit n "" []

-- show polynomial
monoJoin :: (Char, Int) -> String
monoJoin (x, e)
  | e == 0 = ""
  | e == 1 = [x]
  | e > 1 = [x] ++ "^" ++ show e
  | otherwise = [x] ++ "^{" ++ show e ++ "}"

monoShow :: Monomial -> String
monoShow m
  | fst m == 1 =
    if null (snd rm)
      then "1"
      else concatMap monoJoin (snd rm)
  | otherwise = show (fst rm) ++ concatMap monoJoin (snd rm)
  where
    rm = monoReduce m

-- raise a monomial to the exp-power 
monoRaise :: Monomial -> Int -> Maybe Monomial
monoRaise m e
  | e > 0     = Just (monoReduce (fst m ^ e, map (raiser e) (snd m)))
  | otherwise = Nothing
  where
    raiser n z = (fst z, n * snd z)

-- fix it:
monoSumTest :: Monomial -> Monomial -> Either Polynomial Monomial
monoSumTest m n | isMultiple m n = Right (fst m + fst n,snd m)
             | otherwise      = Left [m,n]
-- is more elegant.
-- Currently, monoSum _assumes_ that its arguments are summable
-- sum two Monomials
monoSum' :: Monomial -> Monomial -> Monomial
--this is very inappropriate and should be changed!
monoSum' m n = monoReduce (fst m + fst n, snd m)

monoSum :: [Monomial] -> Monomial
monoSum ms = monoReduce $ foldl monoSum' (0, snd $ head rms) rms
  where
    rms = map monoReduce ms

monoProduct' :: Monomial -> Monomial -> Monomial
monoProduct' p q = monoReduce (fst p * fst q, rleReduce (snd p ++ snd q))

monoProduct :: [Monomial] -> Monomial
monoProduct ms = monoReduce $ foldr (monoProduct' . monoReduce) (1, []) ms

-- reduce a polynomial
polyReduce :: Polynomial -> Polynomial
polyReduce p = filter nonzero $ map monoSum $ groupBy isMultiple $ sortOn snd rp
  where
    nonzero x = fst x /= 0
    rp = map monoReduce p

polyInit :: [(Integer, String, [Int])] -> Polynomial
polyInit = map tupleInit
  where
    tupleInit (x, y, z) = monoInit x y z

polyConst :: Integer -> Polynomial
polyConst n = [monoConst n]

-- sum two polynomials
polySum' :: Polynomial -> Polynomial -> Polynomial
polySum' p q = polyReduce (polyReduce p ++ polyReduce q)

polySum :: [Polynomial] -> Polynomial
polySum ps = polyReduce $ foldl polySum' [(0, [])] rps
  where
    rps = map polyReduce ps

polyProduct' :: Polynomial -> Polynomial -> Polynomial
polyProduct' p q = polyReduce $ concat [[monoProduct' m n | n <- p] | m <- q]

polyProduct :: [Polynomial] -> Polynomial
polyProduct ps = polyReduce $ foldr polyProduct' [(1, [])] ps

-- show a polynomial
polyShow :: Polynomial -> String
polyShow p = intercalate " + " $ map monoShow $ polyReduce rp
  where
    rp = map monoReduce p

polyRaise :: Polynomial -> Int -> Maybe Polynomial
polyRaise p n
  | n > 0 = Just (polyReduce $ polyProduct $ rleExpandPiece (p, n))
  | otherwise = Nothing