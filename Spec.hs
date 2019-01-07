module Multinomiala.Test where
import           Test.Hspec
import           Multinomiala
import Test.QuickCheck
-- once monoInit works fine, monomials can be initialized using it

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
  describe "Monomial reduction" $ do
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
  describe "Monomial power" $ do
  it "(x)^3 = x^3" $ do
    monoRaise b 3 `shouldBe` (1, [('x',3)])


-- def test_init():
--   assert Monomial(1, [('x', 1), ('x', 1)]).vees == ['x']
--   assert Monomial(1, [('x', 1), ('x', 3)]).exps == [3]
--   assert Monomial(1, [('x', 0)]).vees == []
--   assert Monomial(1, [('x', 0)]).exps == []
--   assert Monomial(0, [('x', 1)]).vees == []
--   assert Monomial(0, [('x', 1)]).exps == []

-- def test_pow():
--   assert a ** 3 == Monomial(27,[])
--   assert b ** 3 == Monomial(1, [('x', 3)])
--   assert c ** 2 == Monomial(1, [('y', 2)])
--   assert (d * e)**2 == Monomial(1, [('x', 6), ('y', 8)]) # (x^3y^4)^2
--   assert z**4 == Monomial(0, [])

-- def test_add():
--   pass

-- def test_mul():
--   assert a * a == Monomial(9, [('x', 0)]) #'9'
--   assert a * b == Monomial(3, [('x', 1)]) #'3x'
--   assert b * c == Monomial(1, [('x', 1), ('y', 1)]) #'xy'
--   assert c * d == Monomial(1, [('x', 3), ('y', 1)]) # 'x^3y'
--   assert d * c == Monomial(1, [('x', 3), ('y', 1)]) # 'x^3y'
--   assert g * b == Monomial(3, [('x', 1), ('y', 1)]) # '3xy'
--   assert u * v == Monomial(18, [('y', 3), ('x', 5), ('z', 3)]) #'18x^5y^3z^3'
--   assert a * n == Monomial(9, [('y', -2)])
--   assert m * n == Monomial(-9, [('x', 1), ('y', -2), ('z', -3)])

-- def test_len():
--   assert len(v) == 2
--   assert len(Monomial(3, [('a', 1), ('c', 1), ('b', 1)])) == 3
--   assert len(Monomial(3, [('x', 0)])) == 0

-- def test_is_like():
--   assert b._is_like(d)
--   assert not b._is_like(c)
--   assert c._is_like(e)
--   assert (b * c)._is_like(u)

-- def test_is_multiple():
--   assert f._is_multiple(b)

-- def test_eq():
--   assert Monomial(0, []) == Monomial(0, [('a', 0)])
--   assert Monomial(0, []) == Monomial(0, [('a', 1)])
--   assert Monomial(0, []) == Monomial(0, [('a', 2)])
--   assert (u ** 3) == Monomial(27, [('x', 6), ('y', 9)])
--   assert (u * v) == Monomial(18, [('x', 5), ('y', 3), ('z', 3)])