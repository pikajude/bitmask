{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Bitmask (
    bitmask
) where

import Control.Applicative
import Control.Monad
import Data.Bits (Bits, setBit, testBit, zeroBits)
import Data.Char
import Data.List
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

bitmask :: Name -> Q [Dec]
bitmask name = do
    tycon <- reify name
    boolI <- reify ''Bool
    let TyConI (DataD _ boolName _ _ _) = boolI
        boolTy = ConT boolName
        nonBool (NormalC _ st) = fmap (\(_,b) -> ("unnamed field",b)) $ find (\(_,ty) -> ty /= boolTy) st
        nonBool (RecC _ st) = fmap (\(a,_,c) -> (unName a,c)) $ find (\(_,_,ty) -> ty /= boolTy) st
    case tycon of
        TyConI (DataD _ n _ [con] _) -> do
            let s = unName n
            case nonBool con of
                Nothing -> return ()
                Just (q,t) -> error $ "Field `" ++ q ++ "' of constructor `" ++ s ++ "' has type `" ++ tyName t ++ "' but should have type `Bool'."
            let toName = mkName (decap s ++ "ToBits")
                fromName = mkName ("bitsTo" ++ s)
                (conName, conArity) = argName con
            conArgs <- replicateM conArity (newName "x")
            let vals = zipWith (\a x -> [|if $(varE x) then (`setBit` $(litE (integerL a))) else id|]) [0..] conArgs
                setter = foldl1 (\a b -> [|$(a) . $(b)|]) vals
            toDec <- funD toName
                [clause
                    [conP conName (map varP conArgs)]
                    (normalB [|$(setter) zeroBits|])
                    []]

            i <- newName "i"
            let exprs = map (\a -> [|testBit $(varE i) $(litE (integerL a))|]) (take (length conArgs) [0..])
            fromDec <- funD fromName
                [clause
                    [varP i]
                    (normalB [|$(foldl appE (conE conName) exprs)|])
                    []]
            sig <- sigD toName [t|Bits a => $(conT n) -> a|]
            sig' <- sigD fromName [t|Bits a => a -> $(conT n)|]
            return [sig, toDec, sig', fromDec] -- [toDec, fromDec]
        TyConI (DataD _ (Name (OccName s) _) _ cs _) -> error $ "Expected an ADT with one constructor, but " ++ s ++ " has " ++ show (length cs) ++ "."
        _ -> error "Expected a type constructor, but wasn't passed one."
    where decap (s:ss) = toLower s : ss
          decap [] = []
          argName (NormalC n st) = (n, length st)
          argName (RecC n st) = (n, length st)
          argName (InfixC _ n _) = (n, 2)
          argName (ForallC _ _ c) = argName c
          tyName (ForallT _ _ t) = tyName t
          tyName (AppT a b) = tyName a ++ " -> " ++ tyName b
          tyName (SigT t _) = tyName t
          tyName (VarT n) = unName n
          tyName (ConT n) = unName n
          tyName (PromotedT n) = "'" ++ unName n
          tyName (TupleT s) = "(" ++ replicate s ',' ++ ")"
          tyName (UnboxedTupleT s) = "(#" ++ replicate s ',' ++ "#)"
          tyName ArrowT = "->"
          tyName ListT = "[]"
          tyName (PromotedTupleT s) = "'(" ++ replicate s ',' ++ ")"
          tyName PromotedNilT = "'[]"
          tyName PromotedConsT = "(':)"
          tyName StarT = "*"
          tyName ConstraintT = "Constraint"
          tyName (LitT (NumTyLit i)) = show i
          tyName (LitT (StrTyLit s)) = show s
          unName (Name (OccName n) _) = n
