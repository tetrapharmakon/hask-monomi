import Test.QuickCheck
import Multinomiala

monoGen :: Gen Monomial
monoGen = undefined

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain = forAll monoGen (\p -> monoInit (fst p) (monoExtractVar p) (monoExtractExps p) == p)

-- prop_SumIsAssociative
-- prop_ProdIsAssociative
-- prop_SumIsCommutative
-- prop_ProdIsCommutative
-- prop_ProdDistributes

-- polyRaise [p] n == monoRaise p n


main :: IO ()
main = quickCheck prop_thereAndBackAgain