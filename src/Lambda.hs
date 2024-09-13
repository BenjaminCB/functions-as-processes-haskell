module Lambda where

import Data.Set qualified as Set

data Expr
    = Var String
    | App Expr Expr
    | Abs String Expr
    deriving (Eq)

instance Show Expr where
    show (Var x) = x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")"
    show (Abs x e) = "(\\" ++ x ++ " -> " ++ show e ++ ")"

fv :: Expr -> Set.Set String
fv (Var x) = Set.singleton x
fv (App e1 e2) = Set.union (fv e1) (fv e2)
fv (Abs x e) = Set.filter (/= x) (fv e)

bv :: Expr -> Set.Set String
bv (Var _) = Set.empty
bv (App e1 e2) = Set.union (bv e1) (bv e2)
bv (Abs x e) = Set.insert x (bv e)

v :: Expr -> Set.Set String
v e = Set.union (fv e) (bv e)

subst :: Expr -> String -> Expr -> Expr
subst (Var x) y e = if x == y then e else Var x
subst (App e1 e2) y e = App (subst e1 y e) (subst e2 y e)
subst (Abs x e1) y e2 = if x == y then Abs x e1 else Abs x (subst e1 y e2)

alpha :: String -> String -> Expr -> Expr
alpha y z (Var x) = if x == y then Var z else Var x
alpha y z (App e1 e2) = App (alpha y z e1 ) (alpha y z e2)
alpha y z (Abs x e) = if x == y then Abs z (alpha y z e) else Abs x (alpha y z e)

fresh :: Set.Set String -> String
fresh vars = head $ filter (`Set.notMember` vars) names
    where
        names = [c : s | s <- "" : names, c <- ['a' .. 'z']]

beta :: Expr -> Maybe Expr
beta (App (Abs x e1) e2) = Just $ subst (foldr (\x' e -> alpha x' (fresh allv) e) e1 shadowed) x e2
    where
        shadowed = Set.intersection (bv e1) (fv e2)
        allv = Set.union (v e1) (v e2)
beta (App e1 e2) = flip App e2 <$> beta e1
beta _ = Nothing

betaStar :: Expr -> Expr
betaStar e = maybe e betaStar (beta e)
