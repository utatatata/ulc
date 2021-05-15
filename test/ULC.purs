module Test.ULC where

import Prelude

import Data.List (List(..))
import Data.List as List
import Test.Spec (Spec, describe, it)
import Test.Spec.Assertions (shouldEqual)
import ULC (Term(..), alphaConversion, betaReduction, callByValue, freshVar, fv, isPrenexNF, sbst)

testFv :: Spec Unit
testFv =
  describe "ULC" do
    describe "fv" do
      testValid (Var "x") (List.singleton "x")
      testValid (Var "y") (List.singleton "y")
      testValid (Abst "x" (Var "x")) Nil
      testValid (Abst "x" (Var "y")) (List.singleton "y")
      testValid (App (Var "x") (Var "y")) $ List.fromFoldable ["x", "y"]
  where
  testValid tm result =
    it (show (List.toUnfoldable result :: Array String) <> " ∈ FV(" <> (show tm) <> ")") do
      fv tm `shouldEqual` result

testFreshVar :: Spec Unit
testFreshVar =
  describe "ULC" do
    describe "freshVar" do
      testValid (App (Var "x") (Var "y")) "x" "x0"
      testValid (App (Var "x") (Var "x0")) "x" "x1"
  where
  testValid tm id result =
    it (id <> " ∈ FV(" <> show tm <> ") but " <> result <> " ∉ FV(" <> show tm <> ")") do
      freshVar tm id `shouldEqual` result

testAlphaConversion :: Spec Unit
testAlphaConversion =
  describe "ULC" do
    describe "alphaConversion" do
      testValid (Abst "x" (Var "x")) "y" (Abst "y" (Var "y"))
      testValid (Abst "x" (Var "x")) "x0" (Abst "x0" (Var "x0"))
      testValid (Var "x") "x" (Var "x")
      testValid (Var "x") "y" (Var "x")
      testValid (App (Var "x") (Var "y")) "x" (App (Var "x") (Var "y"))
      testValid (App (Var "x") (Abst "y" (Var "y"))) "y" (App (Var "x") (Abst "y" (Var "y")))
  where
  testValid tm newBind result =
    it (show tm <> " ≡ " <> show result) do
      alphaConversion tm newBind `shouldEqual` result

testSbst :: Spec Unit
testSbst =
  describe "ULC" do
    describe "sbst" do
      testValid (Var "x") "x" (Var "z") (Var "z")
      testValid (Var "x") "y" (Var "z") (Var "x")
      testValid (App (Var "x") (Var "y")) "x" (Var "z") (App (Var "z") (Var "y"))
      testValid (App (Var "x") (Var "y")) "y" (Var "z") (App (Var "x") (Var "z"))
      testValid (Abst "x" (App (Var "x") (Var "y"))) "x" (Var "z") (Abst "x" (App (Var "x") (Var "y")))
      testValid (Abst "x" (App (Var "x") (Var "y"))) "y" (Var "z") (Abst "x" (App (Var "x") (Var "z")))
      testValid (Abst "x" (App (Var "x") (Var "y"))) "y" (Var "x") (Abst "x0" (App (Var "x0") (Var "x")))
  where
  testValid tm id repl result =
    it (show tm <> "[" <> id <> ":=" <> show repl <> "] → " <> show result) do
      sbst tm id repl `shouldEqual` result

testBetaReduction :: Spec Unit
testBetaReduction =
  describe "ULC" do
    describe "betaReduction" do
      testValid (Var "x") (Var "x")
      testValid (Var "y") (Var "y")
      testValid (Abst "x" (Var "x")) (Abst "x" (Var "x"))
      testValid (Abst "x" (App (Abst "x" (Var "x")) (Var "y"))) (Abst "x" (App (Abst "x" (Var "x")) (Var "y")))
      testValid (App (Var "x") (Var "y")) (App (Var "x") (Var "y"))
      testValid (App (Abst "x" (Var "x")) (Var "y")) (Var "y")
      testValid (App (Abst "x" (Abst "y" (App (Var "x") (Var "y")))) (Var "x")) (Abst "y" (App (Var "x") (Var "y")))
      testValid (App (Abst "x" (Abst "y" (App (Var "x") (Var "y")))) (Var "y")) (Abst "y0" (App (Var "y") (Var "y0")))
      testValid (App (Abst "x" (Var "x")) (App (Abst "y" (Var "y")) (Var "x"))) (App (Abst "y" (Var "y")) (Var "x"))
  where
  testValid tm result =
    it (show tm <> " → " <> show result) do
      betaReduction tm `shouldEqual` result

testIsPrenexNF :: Spec Unit
testIsPrenexNF =
  describe "ULC" do
    describe "isPrenexNF" do
      testValid (Var "x")
      testValid (Abst "x" (Var "x"))
      testValid (Abst "x" (App (Var "x") (Var "y")))
      testValid (Abst "x" (App (Abst "x" (Var "x")) (Var "y")))
      testValid (App (Var "x") (Var "y"))
      testInvalid (App (Abst "x" (Var "x")) (Var "y"))
  where
  testValid tm =
    it (show tm <> " is prenex normal form") do
      isPrenexNF tm `shouldEqual` true

  testInvalid tm =
    it (show tm <> " is not prenex normal form") do
      isPrenexNF tm `shouldEqual` false

testCallByValue :: Spec Unit
testCallByValue =
  describe "ULC" do
    describe "callByValue" do
      testValid (App (Abst "x" (Var "x")) (App (Abst "y" (Var "y")) (Var "x"))) (Var "x")
      testValid (App (Var "x") (App (Abst "x" (Var "x")) (Var "y"))) (App (Var "x") (App (Abst "x" (Var "x")) (Var "y")))
  where
  testValid tm result =
    it (show tm <> " →* " <> show result) do
      callByValue tm `shouldEqual` result
