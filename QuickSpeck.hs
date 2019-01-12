import Multinomiala
import Test.Hspec
import Test.Hspec.Core.QuickCheck (modifyMaxSuccess)
import Test.QuickCheck

-- monoGen :: Gen Monomial
-- monoGen = undefined
-- prop_thereAndBackAgain :: Property
-- prop_thereAndBackAgain = forAll monoGen (\p -> monoInit (fst p) (monoExtractVar p) (monoExtractExps p) == p)
main :: IO ()
main =
  hspec $
  describe "Multinomiala" $ do
    modifyMaxSuccess (const 101) $
      it "monoInit is inverse to extract vars and exps" $
      property
        (\p ->
           monoInit (fst p) (monoExtractVar p) (monoExtractExps p) ==
           monoReduce (p :: Monomial))
    it "polySum is associative" $
      property
        (\a b c ->
           polySum'
             (polySum' (a :: Polynomial) (b :: Polynomial))
             (c :: Polynomial) ==
           polySum' a (polySum' b c))
    it "monoProduct is associative" $
      property
        (\a b c ->
           monoProduct'
             (monoProduct' (a :: Monomial) (b :: Monomial))
             (c :: Monomial) ==
           monoProduct' a (monoProduct' b c))
    it "polySum is commutative" $
      property
        (\a b -> polySum' (a :: Polynomial) (b :: Polynomial) == polySum' b a)
    it "monoProduct is commutative" $
      property
        (\a b ->
           monoProduct' (a :: Monomial) (b :: Monomial) == monoProduct' b a)
  -- it "polyProduct is distributive" $ property
  --   (
  --     \a b c -> polySum' (polyProduct' (a :: Polynomial) (b :: Polynomial)) (polyProduct' a (c :: Polynomial)) ==
  --     polyProduct' a (polySum' b c)
  --   )
    it "polyInit coincides with monoInit on singletons" $
      property
        (\n s d ->
           [monoInit (n :: Integer) (s :: String) (d :: [Int])] ==
           polyInit [(n, s, d)])
  -- it "polySub is associative" $ property
  --   (\p q r -> polySub (polySub (p :: Polynomial) (q :: Polynomial)) (r :: Polynomial) == polySub p (polySub q r))
    modifyMaxSuccess (const 200) $
      it "zero is the identity element wrt monoSum" $
      property
        (\m s n ->
           monoSum' (m :: Monomial) (monoInit 0 (s :: String) [n :: Int]) ==
           monoReduce m)
    modifyMaxSuccess (const 200) $
      it "zero is the absorbent element wrt monoProduct" $
      property
        (\m s n ->
           monoProduct' (m :: Monomial) (monoInit 0 (s :: String) [n :: Int]) ==
           (0, []))
    modifyMaxSuccess (const 200) $
      it "zero is the identity element wrt polySum" $
      property (\p -> polySum' (p :: Polynomial) [(0, [])] == polyReduce p)
