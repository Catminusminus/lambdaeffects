module Lib(run) where

import Syntax ( Exp(Assume) )
import Types ( TEnv, Type(TEff) )
import Eval ( Stack, valuable, eval1 )
import TypeCheck ( typeOf, effectful )
import Parse ( parseAll )
import Data.Either ( rights )


assume :: TEnv -> Exp -> TEnv
assume env (Assume x ptype1 ptype2) = (x, (TEff ptype1 ptype2)):env
assume env _ = env

processAssume :: Foldable t => t Exp -> TEnv
processAssume = foldr (flip assume) []

eval :: Exp -> Stack -> Stack -> Exp
eval t s es = go (t, s, es)
  where
    go mod =
      case eval1 mod of
        (v, [], _) | valuable v -> v
        mod' -> go mod'

notAssume :: Exp -> Bool
notAssume (Assume _ _ _) = False
notAssume _ = True

run :: String -> Either String Exp
run strExp = do
    let allAsts = rights $ parseAll strExp
    let tenv = processAssume allAsts
    let ast = head $ filter notAssume allAsts
    case effectful <$> typeOf tenv ast of
        Left err -> Left err
        Right True -> Left "Unhandled Effects exist"
        Right False -> Right $ eval ast [] []
