module Main where

import Lambda qualified as L

main :: IO ()
main = do
    let expr = L.App (L.Abs "x" (L.Var "x")) (L.Var "y")
    print expr

    let expr2 = L.App (L.Abs "x" (L.Abs "y" (L.App (L.Var "x") (L.Var "y")))) (L.Var "y")
    print expr2
    print $ L.beta expr2

    let expr3 = L.App expr2 (L.Var "z")
    print expr3
    print $ L.beta expr3
    print $ L.betaStar expr3
