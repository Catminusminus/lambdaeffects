{-# LANGUAGE LambdaCase #-}

module Eval(eval1, Stack, valuable) where

import Syntax
import Types

type Stack = [Exp -> Exp]

flatfn :: Stack -> Exp -> Exp
flatfn = foldr (.) id

valuable :: Exp -> Bool
valuable = \case
  Var _ -> True
  Handler {} -> True
  Lam {} -> True
  Eff_ _ -> True
  Int _ -> True
  _ -> False

subst :: Exp -> String -> Exp -> Exp
subst (Var x) x' t
  | x == x' = t
  | otherwise = Var x
subst (f :@: a) x t = subst f x t :@: subst a x t
subst (e1 :+: e2) x t = subst e1 x t :+: subst e2 x t
subst (Lam xf ty body) x t
    | xf == x = Lam xf ty body
    | otherwise = Lam xf ty $ subst body x t
subst (WithH h e) x t = WithH (subst h x t) (subst e x t)
subst (Let y e body) x t
  | x == y = Let y (subst e x t) body
  | otherwise = Let y (subst e x t) (subst body x t)
subst (Perform eff e) x t = Perform eff (subst e x t)
subst (Handler eff vh@(xv, t1, ev) effh@(xe, t2, k, t3, t4, ee)) x t = Handler eff vh' effh'
  where
    vh'
      | xv == x = vh
      | otherwise = (xv, t1, subst ev x t)
    effh'
      | xe == x || k == x = effh
      | otherwise = (xe, t2, k, t3, t4, subst ee x t)
subst others _ _ = others

type Env = [(String, Exp)]

hole :: Exp
hole = Var "□"

kfun :: Stack -> Exp
kfun es = do
  let var = "◇"
  Lam var THole $ flatfn es (Var var)

vh :: Exp -> (String, PureType, Exp)
vh (Handler _ it _) = it

effh :: Exp -> (String, PureType, String, PureType, DirtyType, Exp)
effh (Handler _ _ it) = it

type Model = (Exp, Stack, Stack)

binapp :: Exp -> Exp
binapp (Int i :+: Int j) = Int $ i + j

eval1 :: Model -> Model
eval1 (v, f : s, es) | valuable v =  (f v, s, es)
eval1 m@(v, [], _) | valuable v = m
eval1 (Lam x _ body :@: v, s, es) | valuable v = (subst body x v, s, es)
eval1 (f :@: e, s, es)
  | valuable f = (e, (f :@:) : s, es)
  | otherwise = (f, (:@: e) : s, es)
eval1 (e@(e1 :+: e2), s, es)
  | valuable e1 && valuable e2 = (binapp e, s, es)
  | valuable e1 = (e2, (e1 :+:) : s, es)
  | otherwise = (e1, (:+: e2) : s, es)
eval1 (Let x e body, s, es)
  | valuable e = (subst body x e, s, es)
  | otherwise = (e, flip (Let x) body : s, es)
eval1 (WithH h e, s, es)
  | valuable e = do
    let (x, _, ev) = vh h
    (subst ev x e, s, es)
  | otherwise = (e, WithH h : s, es)
eval1 (pf@(Perform eff e), s, es)
  | valuable e = handleOrThrow eff e s es
  | otherwise = (e, Perform eff : s, es)
-- | otherwise = pure (eff, flip Perform e : s, es)
  where
    handleOrThrow eff v s@(f : s') es =
      case f hole of
        WithH (Handler eff' _ (x, _, k, _, _, e)) hole
          | eff' == eff -> do
            let k' = kfun es
                e' = substs e [(x, v), (k, k')]
            (e', s, [])
          | otherwise -> throw
        _ -> throw
      where
        throw = (pf, s', f : es)
    handleOrThrow _ _ [] es = (Abort, s, es)

substs :: Exp -> [(String, Exp)] -> Exp
substs = foldl (\t' (x, u) -> subst t' x u)
