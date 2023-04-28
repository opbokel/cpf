module Cpf (Cpf, CpfError, CpfResult, BrazilianState,
            fromShow, fromShow9, fromShow11, clipTo9Digits,
            getDigit, getDigits, getDigits9, getDigits11,
            validCpfRange, fitIn9Digits, fitIn11Digits,
            appendCheckDigits, validCpf,
            cpfGenerator9, cpfGenerator11, generateCpf9, generateCpf11,
            pretty9, pretty11,
            getStates9, getStates11, getStates
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
    InvalidCpfLength | InvalidCpfValue | InvalidCpfCheckDigits deriving (Show, Enum, Eq)

data BrazilianState = RJ|SP|ES|MG|PR|SC|RS|MS|GO|AC|AL|AP|AM|BA|CE|DF|MA|MT|PA|PB|PE|PI|RN|RO|RR|SE|TO deriving (Show, Enum, Eq)

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

fromShow9 :: Show a => a -> CpfResult
fromShow9 = fromShow (maxLengthNoCheck, maxLengthNoCheck)

fromShow11 :: Show a => a -> CpfResult
fromShow11 = fromShow (maxLength, maxLength)

getDigit :: (Integral a) => Cpf -> Int -> a
getDigit cpf lsdOffset = 
    if inRange (0, maxIndex) lsdOffset
    then fromIntegral $ cpf `div` (10 ^ lsdOffset) `rem` 10 else 0


getDigits :: (Integral a) =>  Int -> Cpf -> [a]
getDigits length cpf = getDigit cpf <$> [length - 1, length - 2 .. 0]

getDigits9 :: (Integral a) => Cpf -> [a]
getDigits9 = getDigits maxLengthNoCheck

getDigits11 :: (Integral a) => Cpf -> [a]
getDigits11 = getDigits maxLength

toString :: Int -> Cpf -> String
toString size cpf = intToDigit <$> getDigits size cpf

toString9 = toString maxLengthNoCheck
toString11 = toString maxLength

appendCheckDigit :: Int -> Cpf -> Cpf
appendCheckDigit length cpf = 
    (cpf * 10) + fromIntegral digit
    where
       multiplyDigit lsdOffset = getDigit cpf lsdOffset * (lsdOffset + 2) 
       digitsSum = sum $ multiplyDigit <$> [0 .. length - 1]
       digit = if digitsSum `mod` 11 < 2 then 0 else 11 - digitsSum `mod` 11

validCpfRange :: Int64 -> Cpf -> CpfResult
validCpfRange maxRangeValue cpf = 
    if inRange (minValue, maxRangeValue) cpf then Right cpf else Left InvalidCpfValue

fitIn9Digits = validCpfRange maxValueNoCheck

fitIn11Digits = validCpfRange maxValue

appendCheckDigits :: Cpf -> CpfResult
appendCheckDigits cpf = appendCheckDigit 10 . appendCheckDigit 9 <$> fitIn9Digits cpf  

validCpf :: Cpf -> CpfResult
validCpf cpf = do
    fitIn11Digits cpf 
    correctCpf <- appendCheckDigits (clipTo9Digits cpf)
    if cpf == correctCpf then Right cpf else Left InvalidCpfCheckDigits


cpfGenerator9 :: Int -> [Cpf]
cpfGenerator9 seed = unfoldr (Just . uniformR (minValue, maxValueNoCheck)) (mkStdGen seed)

cpfGenerator11 :: Int -> [Cpf]
cpfGenerator11 seed  = rights $ appendCheckDigits <$> cpfGenerator9 seed

generateCpf9 :: Int -> Cpf
generateCpf9 = head . take 1 . cpfGenerator9

generateCpf11 :: Int -> Cpf
generateCpf11 = head . take 1 . cpfGenerator11

getDigitChar :: Cpf -> Int -> Char
getDigitChar cpf = intToDigit . getDigit cpf


pretty9 :: Cpf -> String
pretty9 cpf = [d 8, d 7, d 6, '.', d 5, d 4, d 3, '.', d 2, d 1, d 0]
    where
        d = getDigitChar cpf

pretty11 :: Cpf -> String
pretty11 cpf = [d 10, d 9, d 8, '.', d 7, d 6, d 5, '.', d 4, d 3, d 2, '-', d 1, d 0]
    where
        d = getDigitChar cpf


clipTo9Digits :: Cpf -> Cpf
clipTo9Digits cpf = cpf `div` 100

getStates9 :: Cpf -> [BrazilianState]
getStates9 cpf = getStates $ getDigit cpf 0

getStates11 :: Cpf -> [BrazilianState]
getStates11 cpf = getStates $ getDigit cpf 2

getStates :: Int -> [BrazilianState]
getStates 0 = [RS]
getStates 1 = [DF, GO, MT, MS, TO]
getStates 2 = [AC, AP, AM, PA, RO, RR]
getStates 3 = [CE, MA, PI]
getStates 4 = [AL, PB, PE, RN]
getStates 5 = [BA, SE]
getStates 6 = [MG]
getStates 7 = [ES, RJ]
getStates 8 = [SP]
getStates 9 = [PR, SC]
getStates _ = []


    