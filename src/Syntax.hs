module Syntax where

import Types

data Exp
    = Var String
    | Lam String PureType Exp
    | Exp :@: Exp
    | Exp :+: Exp
    | Int Int
    | Eff_ String
    | Handler String (String, PureType, Exp) (String, PureType, String, PureType, DirtyType, Exp)
    | WithH Exp Exp
    | Let String Exp Exp
    | Perform String Exp
    | Assume String PureType PureType
    | Abort
    deriving (Show, Eq)
