module ULC where

import Prelude

import Control.Monad.Trampoline (Trampoline, done, delay, runTrampoline)
import Data.List (List)
import Data.List as List


-- x     ... variable
-- λx.M ... lambda abstraction
-- xy    ... application
data Term
  = Var String
  | Abst String Term
  | App Term Term

derive instance eqTerm :: Eq Term
derive instance ordTerm :: Ord Term

instance showTerm :: Show Term where
  show (Var x) = x
  show (Abst bv body) = "(λ" <> bv <> "." <> show body <> ")"
  show (App m1 m2) = "(" <> show m1 <> " " <> show m2 <> ")"

-- free variables
--
-- e.g.
-- FV(x) = { x }
-- FV(λx.xyz) = { y, z }
-- FV(xy) = { x, y }
fv :: Term -> List String
fv (Var x) = List.singleton x
fv (Abst bv body) = List.filter (\x -> x /= bv) (fv body)
fv (App m1 m2) = fv m1 `List.union` fv m2

-- `variable x is fresh for term tm` means `x ∉ FV(tm)`
--
-- fetchFreshVar tm x = x0 (if x0 ∉ FV(tm))
--                      x1 (else if x1 ∉ FV(tm))
--                      ...
fetchFreshVar :: Term -> String -> String
fetchFreshVar tm x = if List.notElem x fvs then x else recur 0
  where
  fvs = fv tm
  recur n =
    let x' = x <> (show n)
    in if List.notElem x' fvs then x' else recur (n + 1)
  
-- λx.xz ∼ λy.yz (alpha-equivalence)
alphaConversion :: Term -> String -> Term
alphaConversion (Abst bv body) newBv =
  Abst newBv (sbst body bv (Var newBv))
alphaConversion tm _ = tm

-- substitution
sbst :: Term -> String -> Term -> Term
sbst (Var x) id repl | x == id = repl
sbst (Var x) id _ = Var x
sbst (App m1 m2) id repl = App (sbst m1 id repl) (sbst m2 id repl)
sbst (Abst bv body) id _ | bv == id = Abst bv body
sbst (Abst bv body) id repl | bv /= id && List.notElem bv (fv repl) =
  Abst bv (sbst body id repl)
sbst (Abst bv body) id repl =
  let newBv = fetchFreshVar repl bv in
    sbst (alphaConversion (Abst bv body) newBv) id repl

-- e.g.
-- x ▷ x
-- λx.y ▷ λx.y
-- xy ▷ xy
-- ((λx.xy) zz) ▷ zzy
betaReduction :: Term -> Term
betaReduction (App (Abst bv body) m2) = sbst body bv m2
betaReduction tm = tm

-- only the outermost redex
callByValue :: Term -> Term
callByValue tm@(App m1@(Abst _ _) m2) =
  let m2' = callByValue m2 in
  if isValue m2' then
    let tm' = betaReduction (App m1 m2') in
        if tm' == App m1 m2' then
          tm'
        else
      callByValue tm'
  else
    tm
  where
  isValue (App _ _) = false
  isValue _ = true
callByValue tm = tm

-- implementation using Trampoline
-- https://pursuit.purescript.org/packages/purescript-free/6.0.1/docs/Control.Monad.Trampoline
callByValueTrampoline :: Term -> Term
callByValueTrampoline term =
  runTrampoline (recur term)
  where
  recur :: Term -> Trampoline Term
  recur tm@(App m1@(Abst _ _) m2) = do
    m2' <- delay $ \_ -> runTrampoline (recur m2)
    let tm' = betaReduction (App m1 m2')
    if tm' == App m1 m2' then
      done tm'
    else
      recur tm'
  recur tm = done tm
