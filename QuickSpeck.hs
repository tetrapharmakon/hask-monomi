import Test.Hspec
import Test.QuickCheck
import Multinomiala
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)

-- monoGen :: Gen Monomial
-- monoGen = undefined

-- prop_thereAndBackAgain :: Property
-- prop_thereAndBackAgain = forAll monoGen (\p -> monoInit (fst p) (monoExtractVar p) (monoExtractExps p) == p)
main :: IO ()
main = hspec $ 
  describe "Multinomiala" $ do
  modifyMaxSuccess (const 100) $ it "monoInit is inverse to extract vars and exps" $ property $ 
    (\p -> monoInit (fst p) (monoExtractVar p) (monoExtractExps p) == monoReduce (p :: Monomial))
    -- prop_SumIsAssociative
  -- modifyMaxSuccess (const 100) $ it "polySum is associative" $ property $
  --   (\a b c -> polySum' (polySum' (a :: Polynomial) (b :: Polynomial)) (c :: Polynomial) == polySum' a (polySum' b c) )
-- prop_ProdIsAssociative
  -- modifyMaxSuccess (const 100) $ it "monoProduct is associative" $ property $
  --   (\a b c -> monoProduct' (monoProduct' (a :: Monomial) (b :: Monomial)) (c :: Monomial) == monoProduct' a (monoProduct' b c) )
-- -- prop_SumIsCommutative
  -- modifyMaxSuccess (const 100) $ it "polySum is commutative" $ property $
  --   (\a b -> polySum' (a :: Polynomial) (b :: Polynomial) == polySum' b a)
-- -- prop_ProdIsCommutative
  -- modifyMaxSuccess (const 100) $ it "monoProduct is commutative" $ property $
  --   (\a b -> monoProduct' (a :: Monomial) (b :: Monomial) == monoProduct' b a)
-- -- prop_ProdDistributes
  -- modifyMaxSuccess (const 100) $ it "polySum is associative" $ property $
  --   (\a b c -> polySum' (polySum' (a :: Polynomial) (b :: Polynomial)) (c :: Polynomial) == polySum' a (polySum' b c) )
  modifyMaxSuccess (const 100) $ it "polyInit coincides with monoInit on singletons" $ property $
    (\n s d -> [monoInit (n :: Integer) (s :: String) (d :: [Int])] == polyInit [(n, s, d)])
-- polyRaise [p] n == monoRaise p n