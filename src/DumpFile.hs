module DumpFile where

import Structures
import Lib
import Programm
import Data.Word
import OpCode

writeParams :: [Name] -> [Word8]
writeParams [] = []
writeParams (x:xs) = stringToBytes x ++ writeParams xs

writeFunctionsInFile :: [BasicBlock] -> [BasicBlock] -> IO ()
writeFunctionsInFile [] b = return ()
writeFunctionsInFile (x:xs) b = case (instructionsToOpCode2 (instructions x) b) of
  Left i -> appendFile "out.bin" (writeBytes (name x) ++ writeInt (length (params x)) ++ bytesToString (writeParams (params x)) ++ (writeInt (length ((bytesToString i)))) ++ (bytesToString i)) >> writeFunctionsInFile xs b
  Right err -> putStrLn err >> writeFunctionsInFile xs b
  where
    writeBytes s = bytesToString (stringToBytes s)
    writeInt i = bytesToString (intToBytes i)

writeProgramm :: [Word8] -> Programm -> IO ()
writeProgramm c (Programm b e i) = appendFile "out.bin" (bytesToString (intToBytes (length b))) >> writeFunctionsInFile b b >>  appendFile "out.bin" (bytesToString c)
