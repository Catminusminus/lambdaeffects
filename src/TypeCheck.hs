module TypeCheck(typeOf, effectful) where

import Syntax
import Types

uniq :: Eq a => [a] -> [a]
uniq [] = []
uniq (x:xs) = (if x `elem` xs then id else (x:)) $ uniq xs

subset :: [String] -> [String] -> Bool
subset [] _ = True
subset (x:xs) y = if x `elem` y then subset xs y else False

typeOf:: TEnv -> Exp -> Either String Type
typeOf env (Var x) = case lookup x env of
    Just t -> Right t
    Nothing -> Left $ "Unbound Variable: " ++ x
typeOf env (Lam x tx e) = do
    ty <- typeOf ((x, PType tx): env) e
    case ty of
        DType dtype -> Right . PType $ TArrow tx dtype
        PType ptype -> Right . PType $ TArrow tx $ Dirt ptype []
typeOf env (e1 :@: e2) = do
    ty1 <- typeOf env e1
    ty2 <- typeOf env e2
    case (ty1, ty2) of
        (PType (TArrow t1 t2), PType pty2) | t1 == pty2 -> Right $ DType t2
        (PType (TArrow t1 t2), DType (Dirt pty2 [])) | t1 == pty2 -> Right $ DType t2
        _ -> Left $ "Type Mismatch. app" ++ show ty1 ++ show ty2
typeOf env (e1 :+: e2) = do
    ty1 <- typeOf env e1
    ty2 <- typeOf env e2
    case (ty1, ty2) of
        (PType TInt, PType TInt) -> Right . PType $ TInt
        (DType (Dirt TInt []), PType TInt) -> Right . PType $ TInt
        (PType TInt, DType (Dirt TInt [])) -> Right . PType $ TInt
        (DType (Dirt TInt eff), PType TInt) -> Right . DType $ Dirt TInt eff
        (PType TInt, DType (Dirt TInt eff)) -> Right . DType $ Dirt TInt eff
        (DType (Dirt TInt eff1), DType (Dirt TInt eff2)) -> Right . DType $ Dirt TInt $ uniq eff1 ++ eff2
        _ -> Left $ "Type Mismatch. plus" ++ show ty1 ++ show ty2
typeOf _ (Int _) = Right . PType $ TInt
typeOf env (Let x e1 e2) = do
    ty1 <- typeOf env e1
    ty2 <- typeOf ((x, ty1):env) e2
    case (ty1, ty2) of
        (PType _, PType _) -> Right $ ty2
        (DType (Dirt _ effs), PType ptype) -> Right . DType $ Dirt ptype effs
        (PType _, DType (Dirt ptype effs)) -> Right . DType $ Dirt ptype effs
        (DType (Dirt _ effs1), DType (Dirt ptype effs2)) -> Right . DType $ Dirt ptype $ uniq effs1 ++ effs2
typeOf env (WithH e1 e2) = do
    ty1 <- typeOf env e1
    ty2 <- typeOf env e2
    case (ty1, ty2) of
        (PType (THandler t1 t2), DType t3) | t1 == t3 -> Right $ DType t2
        (PType (THandler (Dirt ptype1 effs1) t2), DType (Dirt ptype2 effs2)) | ptype1 == ptype2 && subset effs2 effs1 -> Right $ DType t2
        _ -> Left $ "Type Mismatch. withh"
typeOf env (Perform sigma v) = do
    ty <- typeOf env v
    let eff = lookup sigma env
    case (ty, eff) of
        (PType pty, Just(TEff ptype1 ptype2))| pty == ptype1 ->Right $ DType $ Dirt ptype2 [sigma]
        _ -> Left $ "Type Mismatch. perform"
typeOf env (Handler sigma (x, ptype3, e) (y, ptype4, k, ptype5, dtype1, e')) = do
    ty1 <- typeOf ((x, PType ptype3):env) e
    case (ty1, lookup sigma env) of
        (DType dtype2, Just(TEff ptypel ptyper)) | dtype2 == dtype1 && ptypel == ptype4  && ptyper == ptype5 -> f sigma y ptype4 k dtype1 env e'
        (PType ptype, Just(TEff ptypel ptyper)) | Dirt ptype [] == dtype1 && ptypel == ptype4  && ptyper == ptype5 -> f sigma y ptype4 k dtype1 env e'
        _ -> Left $ "Type Mismatch. handler" ++ (show ty1)
        where 
            f :: String -> String -> PureType -> String -> DirtyType -> TEnv -> Exp -> Either String Type
            f sigma y ptype4 k dtype env e'= do
                ty2 <- typeOf ((y, PType ptype4):(k, PType (TArrow ptype5 dtype1)):env) e'
                case ty2 of
                    DType (Dirt ptype effs) -> Right $ PType $ THandler (Dirt ptype (sigma:effs)) dtype
                    _ -> Left $ "Type Mismatch."

effectful :: Type -> Bool
effectful (DType (Dirt _ [])) = False
effectful (DType (Dirt _ _)) = True
effectful _ = False
