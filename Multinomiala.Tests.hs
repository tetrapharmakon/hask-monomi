module Multinomiala.Tests where

import           Test.Hspec
import           Multinomiala
import           Test.QuickCheck

z = (0,[('x',1)]) -- 0
z1 = (0, [('y',1)]) -- 0
z2 = (0, [('y',4)]) -- 0
uno = (1, [('x',0)]) -- 1
uno' = (1, [('y',0)]) -- 1
a = monoInit 3 "x" [0] -- 3
a' = monoInit 3 "y" [0] -- 3
b = monoInit 1 "x" [1] -- x
c = monoInit 1 "y" [1] -- y
d = monoInit 1 "x" [3] -- x^3
e = monoInit 1 "y" [4] -- y^4
f = monoInit 2 "x" [1] -- 2x
g = monoInit 3 "xy" [2,1] --- 3x^2y
n = monoInit 3 "y" [-2] -- 3y^{-2}
m = monoInit (-3) "xz" [1,-3] -- -3xz^{-3}

u = monoInit 3 "xy" [2,3] -- 3x^2y^3
v = monoInit 6 "xz" [3,3] -- 6x^3z^3

main :: IO ()
main = hspec $ 
  describe "Multinomiala" $ do
  -- monoReduce
  it "3x^0 = 3" $ do
    monoReduce a `shouldBe` (3, [])
    monoReduce z `shouldBe` (0, [])
    monoReduce z1 `shouldBe` (0, [])
    monoReduce z2 `shouldBe` (0, [])
    monoReduce uno `shouldBe` (1, [])
    monoReduce uno' `shouldBe` (1, [])
  it "x^0 = y^0" $ do
    monoReduce uno == monoReduce uno' `shouldBe` True
  it "0x = 0y" $ do
    monoReduce z == monoReduce z1 `shouldBe` True
  it "init a negative coefficient" $ do
    m `shouldBe` (-3, [('x',1),('z',-3)])
  it "x^1x^1x^1 = x^3" $ do
    monoReduce (1, [('x', 1),('x', 1),('x', 1)]) `shouldBe` (1, [('x',3)])
    -- monoMultiply
  it "x  *y = xy" $ do
    monoMultiply b c `shouldBe` (1, [('x', 1), ('y', 1)])
  it "3 * x = 3x" $ do
    monoMultiply a b `shouldBe` (3, [('x', 1)])
  it "-3xz^{-3} * 3y^{-2}=-9xy^{-2}z^{-3}" $ do
    monoMultiply m n `shouldBe` (-9, [('x', 1), ('y', -2), ('z', -3)])
  -- monomial powers
  it "(x)^3 = x^3" $ do
    monoRaise b 3 `shouldBe` (1, [('x',3)])
  it "(x)^1 = x" $ do
    monoRaise b 1 `shouldBe` b
  it "(2x)^3 = 8x^3" $ do
    monoRaise f 3 `shouldBe` (8,[('x',3)])
  -- isMultiple
  it "x and 2x are multiples" $ do
    isMultiple b f `shouldBe` True
  it "y and 3y^{-1} are not multiples" $ do
    isMultiple c n `shouldBe` False  -- isLike
  -- monoExtractVar and
  -- monoExtractExps work fine
  -- together with monoInit
  it "custom monoInit (fst p) (var p) (exps p) == p" $ do
    monoInit (fst m) (monoExtractVar m) (monoExtractExps m) == m `shouldBe` True
    monoInit (fst a) (monoExtractVar a) (monoExtractExps a) == a `shouldBe` True
    monoInit (fst b) (monoExtractVar b) (monoExtractExps b) == b `shouldBe` True
    monoInit (fst c) (monoExtractVar c) (monoExtractExps c) == c `shouldBe` True
    monoInit (fst d) (monoExtractVar d) (monoExtractExps d) == d `shouldBe` True
    monoInit (fst e) (monoExtractVar e) (monoExtractExps e) == e `shouldBe` True
    monoInit (fst u) (monoExtractVar u) (monoExtractExps u) == u `shouldBe` True
    monoInit (fst v) (monoExtractVar v) (monoExtractExps v) == v `shouldBe` True
  -- monoInit
  it "monoInit tested for correct reduction" $ do
    monoInit 0 "xy" [1,1] `shouldBe` (0, [])
    monoInit 3 "" [0] `shouldBe` (3, [])
    monoInit 2 "" [0] `shouldBe` (2, [])
  it "init u is correctly initialized and reduced" $ do
    u `shouldBe` monoInit 3 "xxyyy" [1,1,1,1,1]
  -- monoJoin
  it "monoJoin custom tests" $ do
    monoJoin ('x',1) `shouldBe` "x"
    monoJoin ('a',0) `shouldBe` ""
    monoJoin ('x',1) `shouldBe` "x"
    monoJoin ('x',-3) `shouldBe` "x^{-3}"
  -- monoShow
  it "monoShow tested against custom monomials"  $ do
    monoShow z `shouldBe` "0"
    monoShow z1 `shouldBe` "0"
    monoShow z2 `shouldBe` "0"
    monoShow uno `shouldBe` "1"
    monoShow uno' `shouldBe` "1"
    monoShow a `shouldBe` "3"
    monoShow a' `shouldBe` "3"
    monoShow b `shouldBe` "x"
    monoShow c `shouldBe` "y"
    monoShow d `shouldBe` "x^3"
    monoShow e `shouldBe` "y^4"
    monoShow f `shouldBe` "2x"
    monoShow g `shouldBe` "3x^2y"
    monoShow n `shouldBe` "3y^{-2}"
    monoShow m `shouldBe` "-3xz^{-3}"
    monoShow u `shouldBe` "3x^2y^3"
    monoShow v `shouldBe` "6x^3z^3"
  -- monoRaise
  it "monoRaise tested against custom monomials"  $ do
    monoRaise z 3 `shouldBe` (0, [])
    monoRaise z1 1 `shouldBe` (0, [])
    monoRaise z2 2 `shouldBe` (0, [])
    monoRaise uno 3 `shouldBe` (1,[])
    monoRaise uno' 1 `shouldBe` (1,[])
    monoRaise a 2 `shouldBe` (9,[])
    monoRaise a' 3 `shouldBe` (27, [])
    monoRaise b 3 `shouldBe` (1, [('x',3)])
    monoRaise c 1 `shouldBe` (1, [('y',1)])
    monoRaise d 2 `shouldBe` monoInit 1 "x" [6]
    monoRaise e 3 `shouldBe` (1, [('y',12)])
    monoRaise f 1 `shouldBe` f
    monoRaise g 2 `shouldBe` (9, [('x',4),('y',2)])
    monoRaise n 3 `shouldBe` (27, [('y', -6)])
    monoRaise m 1 `shouldBe` monoInit (-3) "xz" [1,-3]
    monoRaise u 2 `shouldBe` monoInit 9 "xy" [4,6]
    monoRaise v 3 `shouldBe` monoInit (6^3) "xz" [9,9]
  -- monoSum
  it "monoSum tested against custom monomials"  $ do
    monoSum [z,z] `shouldBe` (0,[])
    monoSum [z1,z2] `shouldBe` (0,[])
    monoSum [z2,z2] `shouldBe` (0,[])
    monoSum [b,f] `shouldBe` (3,[('x',1)])
    monoSum [b,f,b,f,f,f] `shouldBe` (10,[('x',1)])
    monoSum [e,(7,[('y',4)])] `shouldBe` (8,[('y',4)])
  -- monoProduct
  it "monoProduct tested against custom monomials"  $ do
    monoProduct [a,b,f] `shouldBe` (6,[('x',2)])
    monoProduct [d,e,f] `shouldBe` (2,[('x',4),('y',4)])
    monoProduct [z,a] `shouldBe` (0,[])
    monoProduct [z1,b] `shouldBe` (0,[])
    monoProduct [z,f] `shouldBe` (0,[])
    monoProduct [a,b,c,d,e,f,u,v] `shouldBe` (108,[('x',10),('y',8),('z',3)])
  -- polyReduce
  it "polyReduce tested against custom monomials"  $ do
    polyReduce [z,z1,z2] `shouldBe` []
    polyReduce [a,b,f] `shouldBe` [monoInit 3 "" [], monoInit 3 "x" [1]]
  it "polyRaise tested against custom monomials"  $ do
    polyRaise [z] 3 `shouldBe` []
    polyRaise [b,c] 2 `shouldBe` polyReduce [(1,[('x',2)]),(2,[('x',1),('y',1)]),(1,[('y',2)])]
    -- polySum
  -- ...
  -- polyShow
  -- ...

polySub "x^2y + 3xy" [("x" -> "s+1"), ("y" -> "st")]

-- runQc :: IO ()
-- runQc = quickCheck polyReduce