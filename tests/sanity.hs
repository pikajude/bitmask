{-# OPTIONS_GHC -ddump-splices #-}
{-# LANGUAGE TemplateHaskell #-}

import Control.Applicative
import Data.Bitmask
import Data.Int
import Data.Word
import Test.QuickCheck

data Test = Test
          { test1 :: Bool
          , test2 :: ()
          , test3 :: Bool
          , test4 :: Bool
          } deriving (Show, Eq)

instance Arbitrary Test where
    arbitrary = Test <$> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary

bitmask ''Test

main :: IO ()
main = do
    quickCheck (\x -> bitsToTest (testToBits x :: Word8) == x)
    quickCheck (\x -> bitsToTest (testToBits x :: Word16) == x)
    quickCheck (\x -> bitsToTest (testToBits x :: Word32) == x)
    quickCheck (\x -> bitsToTest (testToBits x :: Word64) == x)
    quickCheck (\x -> bitsToTest (testToBits x :: Int64) == x)
