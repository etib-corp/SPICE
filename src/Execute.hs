module Execute where

import Structures
import Data.Word (Word8)
import Data.List (unfoldr)
import Debug.Trace (trace)
import Data.Char (isHexDigit)
import OpCode
import Lib
import Programm

readName :: [Word8] -> (String, [Word8])
readName [] = ([], [])
readName b = (getName b, drop (length (getName b) + 1) b)

readInt :: [Word8] -> (Int, [Word8])
readInt (x1:x2:xs) = (fromIntegral x1 + fromIntegral x2 * 256, xs)

readParams :: [Word8] -> Int -> [String] -> ([String], [Word8])
readParams b 0 p = (p, b)
readParams b n p = readParams rest (n - 1) (p ++ [name])
        where
            (name, rest) = readName b

readInstructions :: [Word8] -> Int -> ([Instruction], [Word8])
readInstructions b 0 = ([], b)
readInstructions b n = (opCodesToInstructions (take n b), drop n b)

readBasicBlock :: [Word8] -> (BasicBlock, [Word8])
readBasicBlock [] = (emptyBasicBlock, [])
readBasicBlock b = (BasicBlock name instructions params, rest)
    where
        (name, rest1) = readName b
        (n, rest2) = readInt rest1
        (params, rest3) = readParams rest2 n []
        (m, rest4) = readInt rest3
        (instructions, rest) = readInstructions rest4 m

readBasicBlocks :: [Word8] -> Int -> ([BasicBlock], [Word8]) -> ([BasicBlock], [Word8])
readBasicBlocks b 0 (blocks, rest) = (blocks, rest)
readBasicBlocks b n (blocks, rest) = readBasicBlocks rest1 (n - 1) (blocks ++ [basicBlock], rest1)
    where
        (basicBlock, rest1) = readBasicBlock b

secureReadBasicBlocks :: [Word8] -> ([BasicBlock], [Word8])
secureReadBasicBlocks b = case readInt b of
    (0, rest) -> ([], rest)
    (n, rest) -> readBasicBlocks rest n ([], [])

executeVM :: String -> IO ()
executeVM content = case secureReadBasicBlocks c of
    (blocks, rest) -> case programmEval (Programm blocks emptyEnv (opCodesToInstructions rest)) [] of
        Right (p, s) -> return ()
        Left m -> print m
    where
        b = stringToBytes content
        c = take ((length b) - 1) b
