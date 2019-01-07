{-# LANGUAGE RecordWildCards #-}

import           Test.Hspec
import           Multinomiala
import           Test.QuickCheck

a = monoInit 3 "x" [0]
a' = monoInit 3 "y" [0]
b = monoInit 1 "x" [1]
c = monoInit 1 "y" [1]
d = monoInit 1 "x" [3]
e = monoInit 1 "y" [4]
f = monoInit 2 "x" [1]
g = monoInit 3 "y" [1]
n = monoInit 3 "y" [-2]
m = monoInit (-3) "xz" [1,-3]

u = monoInit 3 "xy" [2,3]
v = monoInit 6 "xz" [3,3]

main :: IO ()
main = hspec $ 
  describe "Multinomiala" $ do
  it "3x^0 = 3" $ do
    monoReduce a `shouldBe` (3, [])
  it "init a negative coefficient" $ do
    m `shouldBe` (-3, [('x',1),('z',-3)])
  it "x^1x^1x^1 = x^3" $ do
    monoReduce (1, [('x', 1),('x', 1),('x', 1)]) `shouldBe` (1, [('x',3)])
  it "x  *y = xy" $ do
    monoMultiply b c `shouldBe` (1, [('x', 1), ('y', 1)])
  it "3 * x = 3x" $ do
    monoMultiply a b `shouldBe` (3, [('x', 1)])
  it "-3xz^{-3} * 3y^{-2}=-9xy^{-2}z^{-3}" $ do
    monoMultiply m n `shouldBe` (-9, [('x', 1), ('y', -2), ('z', -3)])
  -- monomial powers
  it "(x)^3 = x^3" $ do
    monoRaise b 3 `shouldBe` (1, [('x',3)])