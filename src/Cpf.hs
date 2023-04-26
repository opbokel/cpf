module Cpf (CpfError,
            Cpf,
            -- ValidatedResult,
            -- fromString,
            -- validFormat11,
            -- validFormat9,
            -- appendCheckDigits,
            -- validCpf,
            -- cpfGenerator,
            -- pretty,
            -- fromShow11,
            -- fromShow9
            ) where

import Prelude
import Data.Char
import Data.List
import Data.Maybe
import System.Random 
import Data.Either
import Data.Int (Int64, Int8)
import Data.Ix (inRange)

data CpfError = 
    InvalidCpfLength | 
    InvalidCpfValue | 
    InvalidCpfCheckDigits deriving Show

type Cpf = Int64

type CpfResult = Either CpfError Cpf

maxLength :: Int
maxLength = 11

maxIndex :: Int
maxIndex = maxLength - 1

maxLengthNoCheck :: Int
maxLengthNoCheck = 9

maxValueNoCheck :: Int64
maxValueNoCheck = (10 ^ maxLengthNoCheck) - 1

minValue :: Int64
minValue = 1

maxValue :: Int64
maxValue = (10 ^ maxLength) - 1

fromShow :: Show a => (Int, Int) -> a -> CpfResult
fromShow range cpf = 
    let digits = filter isDigit (show cpf)
    in if inRange range $ length digits
       then Right (read digits) else Left InvalidCpfLength


fromShow11 :: Show a => a -> CpfResult
fromShow11 = fromShow (maxLength, maxLength)

fromShow9 :: Show a => a -> CpfResult
fromShow9 = fromShow (maxLengthNoCheck, maxLengthNoCheck)


getDigit :: (Integral a) => Cpf -> Int -> a
getDigit cpf lsdOffset = 
    if inRange (0, maxIndex) lsdOffset
    then fromIntegral $ cpf `div` (10 ^ lsdOffset) `rem` 10 else 0


getDigits :: (Integral a) =>  Int -> Cpf -> [a]
getDigits length cpf = getDigit cpf <$> [length - 1, length - 2 .. 0]

-- getDigits11 :: (Integral a) => Cpf -> [a]
getDigits11 = getDigits maxLength
getDigits9 = getDigits maxLengthNoCheck

toString :: Int -> Cpf -> String
toString size cpf = intToDigit <$> getDigits size cpf

toString11 = toString maxLength
toString9 = toString maxLengthNoCheck

appendCheckDigit :: Int -> Cpf -> Cpf
appendCheckDigit length cpf = 
    (cpf * 10) + fromIntegral digit
    where
       multiplyDigit lsdOffset = getDigit cpf lsdOffset * (lsdOffset + 2) 
       digitsSum = sum $ multiplyDigit <$> [0 .. length - 1]
       digit = if digitsSum `mod` 11 < 2 then 0 else 11 - digitsSum `mod` 11

validCpfRange :: Int64 -> Cpf -> CpfResult
validCpfRange maxRangeValue cpf = if inRange (minValue, maxRangeValue) cpf then Right cpf else Left InvalidCpfValue

fitIn9Digits = validCpfRange maxValueNoCheck

fitIn11Digits = validCpfRange maxValue

appendCheckDigits :: Cpf -> CpfResult
appendCheckDigits cpf = appendCheckDigit 10 . appendCheckDigit 9 <$> fitIn9Digits cpf  

validCpf :: Cpf -> CpfResult
validCpf cpf = do
    fitIn11Digits cpf 
    correctCpf <- appendCheckDigits (cpf `div` 100)
    if cpf == correctCpf then Right cpf else Left InvalidCpfCheckDigits


cpfGenerator9 :: Int -> [Cpf]
cpfGenerator9 seed = unfoldr (Just . uniformR (minValue, maxValueNoCheck)) (mkStdGen seed)

cpfGenerator11 :: Int -> [Cpf]
cpfGenerator11 seed  = rights $ appendCheckDigits <$> cpfGenerator9 seed

getDigitChar :: Cpf -> Int -> Char
getDigitChar cpf = intToDigit . getDigit cpf

pretty9 :: Cpf -> String
pretty9 cpf = [d 8, d 7, d 6, '.', d 5, d 4, d 3, '.', d 2, d 1, d 0]
    where
        d = getDigitChar cpf

pretty11 :: Cpf -> String
pretty11 cpf =  [d 10, d 9, d 8, '.', d 7, d 6, d 5, '.', d 4, d 3, d 2, '-', d 1, d 0]
    where
        d = getDigitChar cpf

    