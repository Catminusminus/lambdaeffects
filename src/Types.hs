module Types where

data DirtyType
    = Dirt PureType [String]
    deriving (Show, Eq)

data PureType
    = TInt
    | TArrow PureType DirtyType
    | THandler DirtyType DirtyType
    | THole
    deriving (Show, Eq)

data Type
    = PType PureType
    | DType DirtyType
    | TEff PureType PureType
    deriving (Show, Eq)

type TEnv = [(String, Type)]
