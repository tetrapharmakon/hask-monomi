import Test.QuickCheck
import Multinomiala

monoGen :: Gen Monomial
monoGen = 

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
forAll monoGen (\p -> monoInit (fst p) (monoExtractVar p) (monoExtractExps p) == p)

main :: IO ()
main = quickCheck prop_thereAndBackAgain