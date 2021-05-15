module Main where

import Prelude

import Effect (Effect)
import Effect.Console (logShow)
import ULC (Term(..), callByValue)

main :: Effect Unit
main = do
  logShow (callByValue (Var "x"))
  logShow (callByValue (App (Var "x") (Var "y")))
  logShow (callByValue (Abst "x" (Var "x")))
  logShow (callByValue (App (Abst "x" (Var "x")) (Var "y")))
  logShow (callByValue (App (Abst "x" (Var "x")) (App (Abst "x" (Abst "y" (App (Var "x") (Var "y")))) (App (Var "x") (Var "y")))))
