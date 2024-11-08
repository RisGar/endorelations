module Main where

import Test.Hspec (describe, hspec, it, shouldBe)

type Relation a = (a, a)

-- every node has a loop
reflexive :: (Eq a) => [Relation a] -> Bool
reflexive xs = all (\(a, b) -> ((a, a) `elem` xs) && ((b, b) `elem` xs)) xs

-- no node has a loop
irreflexive :: (Eq a) => [Relation a] -> Bool
irreflexive = all (uncurry (/=))

-- between any two nodes either both edges or none
symmetric :: (Eq a) => [Relation a] -> Bool
symmetric xs = all (\(x, y) -> (y, x) `elem` xs) xs

-- between two distinct nodes at most one edge
antisymmetric :: (Eq a) => [Relation a] -> Bool
antisymmetric xs = all (\(x, y) -> (y, x) `notElem` xs || (x == y)) xs

-- no loops, between two distinct nodes at most one edge
asymmetric :: (Eq a) => [Relation a] -> Bool
asymmetric xs = all (\(x, y) -> (y, x) `notElem` xs) xs

-- wherever you can get in two steps, you can get in one step
transitive :: (Eq a) => [Relation a] -> Bool
transitive xs = all (\(a, b) -> all (\(_, d) -> (a, d) `elem` xs) (filter (\(c, _) -> b == c) xs)) xs

endorelations :: (Show a, Eq a) => [Relation a] -> IO ()
endorelations val = do
  print $ "reflexive " ++ show (reflexive val)
  print $ "irreflexive " ++ show (irreflexive val)
  print $ "symmetric " ++ show (symmetric val)
  print $ "antisymmetric " ++ show (antisymmetric val)
  print $ "asymmetric " ++ show (asymmetric val)
  print $ "transitive " ++ show (transitive val)

-- TODO empty set
main :: IO ()
main = hspec $ do
  describe "reflexive" $ do
    it "returns True for a set with only loops" $
      reflexive ([(1, 1), (2, 2), (3, 3)] :: [(Int, Int)]) `shouldBe` True

    it "returns True for a set with loops and other relations" $
      reflexive ([(1, 1), (2, 2), (3, 3), (3, 2)] :: [(Int, Int)]) `shouldBe` True

    it "returns False for a set with no loops" $
      reflexive ([(1, 2), (2, 3), (3, 4)] :: [(Int, Int)]) `shouldBe` False

    it "returns False for a set with only some loops" $
      reflexive ([(1, 1), (2, 3), (3, 3)] :: [(Int, Int)]) `shouldBe` False

    it "returns True for an empty set" $
      reflexive ([] :: [(Int, Int)]) `shouldBe` True

  describe "irreflexive" $ do
    it "returns True for a set with no loops" $
      irreflexive ([(1, 2), (2, 3), (3, 4)] :: [(Int, Int)]) `shouldBe` True

    it "returns False for a set with one loop" $
      irreflexive ([(1, 1), (2, 3), (3, 4)] :: [(Int, Int)]) `shouldBe` False

    it "returns False for a set with only loops" $
      irreflexive ([(1, 1), (2, 2), (3, 3)] :: [(Int, Int)]) `shouldBe` False

    it "returns True for an empty set" $
      irreflexive ([] :: [(Int, Int)]) `shouldBe` True

  describe "symmetric" $ do
    it "returns True when there are both edges between all nodes" $
      symmetric ([(1, 2), (2, 1), (3, 4), (4, 3)] :: [(Int, Int)]) `shouldBe` True

    it "returns True when there are only loops" $
      symmetric ([(1, 1), (2, 2), (3, 3)] :: [(Int, Int)]) `shouldBe` True

    it "returns True when there are some loops and both edges between other nodes" $
      symmetric ([(1, 1), (2, 2), (3, 4), (4, 3)] :: [(Int, Int)]) `shouldBe` True

    it "returns True when there are both edges between all nodes and loops" $
      symmetric ([(1, 2), (2, 1), (3, 4), (4, 3), (3, 3), (1, 1)] :: [(Int, Int)]) `shouldBe` True

    it "returns False when there are two nodes which only have one edge between them" $
      symmetric ([(1, 2), (2, 1), (3, 4), (4, 3), (2, 3)] :: [(Int, Int)]) `shouldBe` False

    it "returns True for an empty set" $
      symmetric ([] :: [(Int, Int)]) `shouldBe` True

  describe "antisymmetric" $ do
    it "returns True for a set with only one edge between two distinct nodes" $
      antisymmetric ([(1, 2), (3, 4), (2, 3)] :: [(Int, Int)]) `shouldBe` True

    it "returns True for a set with only loops" $
      antisymmetric ([(1, 1), (2, 2)] :: [(Int, Int)]) `shouldBe` True

    it "returns True for a set with loops and only one edge between two distinct nodes" $
      antisymmetric ([(1, 1), (2, 3), (3, 4), (1, 3)] :: [(Int, Int)]) `shouldBe` True

    it "returns False for a set with two edges between distinct nodes" $
      antisymmetric ([(1, 2), (2, 1)] :: [(Int, Int)]) `shouldBe` False

    it "returns True for an empty set" $
      antisymmetric ([] :: [(Int, Int)]) `shouldBe` True

  describe "asymmetric" $ do
    it "returns True for a set with only one edge between two distinct nodes" $
      asymmetric ([(1, 2), (3, 4), (2, 3)] :: [(Int, Int)]) `shouldBe` True

    it "returns False for a set with two edges between distinct nodes" $
      asymmetric ([(1, 2), (2, 1), (4, 3)] :: [(Int, Int)]) `shouldBe` False

    it "returns False for a set with loops and only one edge between two distinct nodes" $
      asymmetric ([(1, 1), (2, 3), (4, 3), (3, 3)] :: [(Int, Int)]) `shouldBe` False

    it "returns False for a set with only loops" $
      asymmetric ([(1, 1), (2, 2)] :: [(Int, Int)]) `shouldBe` False

    it "returns True for an empty set" $
      asymmetric ([] :: [(Int, Int)]) `shouldBe` True

  describe "transitive" $ do
    it "returns True for an empty set" $
      transitive ([] :: [(Int, Int)]) `shouldBe` True

    it "returns True when there is an edge between two nodes that have two edges between them" $
      transitive ([(1, 2), (2, 3), (1, 3)] :: [(Int, Int)]) `shouldBe` True

    it "returns False when there is no edge between two nodes that have two edges between them" $
      transitive ([(1, 2), (2, 3)] :: [(Int, Int)]) `shouldBe` False

    it "returns False when there is only sometimes an edge between two nodes that have two edges between them" $
      transitive ([(1, 2), (2, 3), (1, 3), (2, 4)] :: [(Int, Int)]) `shouldBe` False

    it "returns True for an empty set" $
      asymmetric ([] :: [(Int, Int)]) `shouldBe` True
