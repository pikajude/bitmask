{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}

module Data.Bitmask (
  mkBitmask
) where

import Control.Monad
import Data.Bits (Bits, setBit, testBit, zeroBits)
import Data.Char
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

mkBitmask :: Name -> Q [Dec]
mkBitmask name = do
    tycon <- reify name
    case tycon of
        TyConI (DataD _ n _ [con] _) -> genBitmask n con
        TyConI (DataD _ (Name (OccName s) _) _ cs _) -> error $ "Expected an ADT with one constructor, but " ++ s ++ " has " ++ show (length cs) ++ "."
        _ -> error "Expected a type constructor, but wasn't passed one."

genBitmask :: Name -> Con -> Q [Dec]
genBitmask name constructor = do
    let conString = unName name
    isBool <- allBool constructor
    unless isBool $
        error $ "Constructor `" ++ conString ++ "' has a non-Bool argument."
    let toName = mkName (decapitalize conString ++ "ToBits")
        fromName = mkName ("bitsTo" ++ conString)
        (conName, conArity) = argumentInfo constructor
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
    sig <- sigD toName [t|Bits a => $(conT name) -> a|]
    sig' <- sigD fromName [t|Bits a => a -> $(conT name)|]
    return [sig, toDec, sig', fromDec] -- [toDec, fromDec]
    where
        unName (Name (OccName n) _) = n

allBool :: Con -> Q Bool
allBool (NormalC _ st) = fmap (\b -> all ((== b) . snd) st) boolTypeName
allBool (RecC _ st) = fmap (\b -> all (\(_,_,c) -> c == b) st) boolTypeName
allBool (InfixC (_,a) _ (_,b)) = fmap (\bt -> a == bt && b == bt) boolTypeName
allBool (ForallC _ _ con) = allBool con

boolTypeName :: Q Type
boolTypeName = do
    boolI <- reify ''Bool
    let TyConI (DataD _ boolName _ _ _) = boolI
    return $ ConT boolName

argumentInfo :: Con -> (Name, Int)
argumentInfo (NormalC n st) = (n, length st)
argumentInfo (RecC n st) = (n, length st)
argumentInfo (InfixC _ n _) = (n, 2)
argumentInfo (ForallC _ _ c) = argumentInfo c

decapitalize :: String -> String
decapitalize (s:ss) = toLower s : ss
decapitalize [] = error "decapitalize: empty string"
