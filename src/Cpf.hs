module Cpf (CpfError,
            Cpf,
            ValidatedResult,
            fromString,
            validFormat11,
            validFormat9,
            appendCheckDigits,
            validCpf,
            cpfGenerator,
            pretty,
            fromShow11,
            fromShow9
            ) where

import Prelude
import Data.Char
import Data.List
import Data.Maybe
import System.Random 
import Data.List.Split
import Data.Either

data CpfError = InvalidCpfLenght | InvalidCpfDigit | InvalidCpfCheckDigit deriving Show

type Cpf = [Int]

type ValidatedResult = Either CpfError Cpf

lengthWithCheckDigits = 11
lengthNoCheckDigits = 9

fromString :: String -> Cpf
fromString = fmap digitToInt . filter isDigit

validFormat :: Int -> Cpf -> ValidatedResult
validFormat expectedLenght cpf = validLength >> validDigits
    where
        validDigits = if isJust (find (\d -> d < 0 || d > 9) cpf) then Left InvalidCpfDigit else Right cpf
        validLength = if expectedLenght == length cpf then Right cpf else Left InvalidCpfLenght

validFormat11 = validFormat lengthWithCheckDigits

validFormat9 = validFormat lengthNoCheckDigits

calculateCheckDigit :: Int -> Cpf -> Int
calculateCheckDigit firstMultiplier cpf =
   if mod digitsSum 11 < 2 then 0 else 11 - mod digitsSum 11
   where
       digitsSum = sum $ uncurry (*) <$> take (firstMultiplier - 1) (zip [firstMultiplier, firstMultiplier - 1 .. 2] cpf)

appendCheckDigits :: Cpf -> ValidatedResult
appendCheckDigits cpf = do
    validFormat9 cpf
    let checkDigit1 = calculateCheckDigit 10 cpf
    let checkDigit2 = calculateCheckDigit 11 (cpf ++ [checkDigit1])
    return (cpf ++ [checkDigit1, checkDigit2])

validCpf :: Cpf -> ValidatedResult
validCpf cpf = do
    validFormat11 cpf 
    cpfWithGeneratedDigits <- appendCheckDigits (take lengthNoCheckDigits cpf)
    if cpf == cpfWithGeneratedDigits then Right cpf else Left InvalidCpfCheckDigit

cpfGenerator :: Int -> [Cpf]
cpfGenerator seed = rights $ appendCheckDigits <$> chunksOf lengthNoCheckDigits digits
    where
        digits = unfoldr (Just . uniformR (0, 9)) (mkStdGen seed)

toString :: Cpf -> String
toString = fmap intToDigit

pretty :: Cpf -> String
pretty cpf = intercalate "" $ separate <$> zip [0..] (chunksOf 3 $ toString cpf)
    where
        separate (index, chunk) 
            | index == 3 = '-' : chunk
            | index > 0 && index < 3 = '.' : chunk
            | otherwise = chunk

fromShow :: Show a => Int -> a -> Cpf
fromShow minSize showableCpf = 
    let cpf = fromString (show showableCpf)
        missingZeros = minSize - length cpf
    in if missingZeros > 0 then replicate missingZeros 0 ++ cpf else cpf

fromShow11 :: Show a => a -> Cpf
fromShow11 = fromShow lengthWithCheckDigits

fromShow9 :: Show a => a -> Cpf
fromShow9 = fromShow lengthNoCheckDigits
    