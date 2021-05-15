module ULC where

import Prelude

import Data.Maybe (Maybe(..))
import Data.List (List(..))
import Data.List as List

data Term
  = Var String
  | Abst String Term
  | App Term Term

derive instance eqTerm :: Eq Term
derive instance ordTerm :: Ord Term

instance showTerm :: Show Term where
  show (Var x) = x
  show (Abst bind body) = "(λ" <> bind <> "." <> show body <> ")"
  show (App m1 m2) = "(" <> show m1 <> " " <> show m2 <> ")"

fv :: Term -> List String
fv (Var x) = List.singleton x
fv (Abst bind body) = List.filter (\x -> x /= bind) (fv body)
fv (App m1 m2) = List.union (fv m1) (fv m2)

freshVar :: Term -> String -> String
freshVar tm x = if List.notElem x fvs then x else recur 0
  where
  fvs = fv tm
  recur n =
    let x' = x <> (show n)
    in if List.notElem x' fvs then x' else recur (n + 1)
  
alphaConversion :: Term -> String -> Term
alphaConversion (Abst bind body) newBind =
  Abst newBind (sbst body bind (Var newBind))
alphaConversion tm _ = tm

sbst :: Term -> String -> Term -> Term
sbst (Var x) id repl | x == id = repl
sbst (Var x) id _ = Var x
sbst (App m1 m2) id repl = App (sbst m1 id repl) (sbst m2 id repl)
sbst (Abst bind body) id _ | bind == id = Abst bind body
sbst (Abst bind body) id repl | bind /= id && List.notElem bind (fv repl) =
  Abst bind (sbst body id repl)
sbst (Abst bind body) id repl =
  let newBind = freshVar repl bind in
    sbst (alphaConversion (Abst bind body) newBind) id repl

betaReduction :: Term -> Term
betaReduction (App (Abst bind body) m2) = sbst body bind m2
betaReduction tm = tm

isPrenexNF :: Term -> Boolean
isPrenexNF (App (Abst _ _) _) = false
isPrenexNF _ = true

callByValue :: Term -> Term
callByValue tm =
  let tm' = betaReduction tm in
    if isPrenexNF tm' then tm' else callByValue tm'