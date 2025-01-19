module OpCode where

import Structures
import Data.Word
import Debug.Trace

intToBytes :: Int -> [Word8]
intToBytes i = map fromIntegral [i `mod` 256, i `div` 256]

stringToBytes :: String -> [Word8]
stringToBytes s = map (fromIntegral . fromEnum) s <> [0]

instructionToOpCode :: Instruction -> [Word8]
instructionToOpCode (PushInt i) = [0x01] ++ intToBytes i
instructionToOpCode (Get v) = [0x03] ++ stringToBytes v
instructionToOpCode Add = [0x04]
instructionToOpCode Sub = [0x05]
instructionToOpCode Mul = [0x06]
instructionToOpCode Div = [0x07]
instructionToOpCode Eq = [0x08]
instructionToOpCode NEq = [0x09]
instructionToOpCode Less = [0x0A]
instructionToOpCode LessEq = [0x0B]
instructionToOpCode (Jump i) = [0x0C] ++ intToBytes i
instructionToOpCode (JumpIf i) = [0x0D] ++ intToBytes i
instructionToOpCode (JumpIfNot i) = [0x0F] ++ intToBytes i
instructionToOpCode (Return) = [0x10]
instructionToOpCode (Pop) = [0x12]
instructionToOpCode (PopN i) = [0x13] ++ intToBytes i
instructionToOpCode (None) = [0x14]
instructionToOpCode (Print) = [0x15]

getName :: [Word8] -> String
getName [] = []
getName (0:is) = []
getName (i:is) = toEnum (fromIntegral i) : getName is

getVariable :: [Word8] -> Maybe Int
getVariable [] = Nothing
getVariable (0x0F:is) = Nothing
getVariable (i1:i2:is) = Just (fromIntegral i1 + fromIntegral i2 * 256)

separateParams :: [Word8] -> [[Word8]]
separateParams input = go [] [] input
  where
    go current acc []
        | null current = reverse acc              -- If the current group is empty, don't add it
        | otherwise    = reverse (reverse current : acc) -- Add the last group if not empty
    go current acc (0xFF:is) = go [] (reverse current : acc) is -- Add the group and reset
    go current acc (x:is) = go (x : current) acc is -- Add the current element to the current group

getParamsInstruction :: [Word8] -> [[Instruction]]
getParamsInstruction [] = []
getParamsInstruction l = map opCodesToInstructions (separateParams l)
  where
    test = separateParams l

opCodesToInstructions :: [Word8] -> [Instruction]
opCodesToInstructions [] = []
opCodesToInstructions (0x14:is) = None : opCodesToInstructions is
opCodesToInstructions (0x13:i1:i2:is) = PopN (fromIntegral i1 + fromIntegral i2 * 256) : opCodesToInstructions is
opCodesToInstructions (0x12:is) = Pop : opCodesToInstructions is
-- opCodesToInstructions (0x11:n:ns) = CallFunc (map (toEnum . fromIntegral) (n:ns)) : opCodesToInstructions is
opCodesToInstructions (0x10:is) = Return : opCodesToInstructions is
opCodesToInstructions (0x0F:i1:i2:is) = JumpIfNot (fromIntegral i1 + fromIntegral i2 * 256) : opCodesToInstructions is
-- opCodesToInstructions (0x0E:n:ns) =
opCodesToInstructions (0x0E:l) = Set name (case var of
    Just i -> IntValue i
    Nothing -> PopValue
    ) : opCodesToInstructions (drop (length name + (case (var) of
        Nothing -> 2
        _ -> 3)) l)
                                    where
                                        name = getName l
                                        var = getVariable (drop ((length name) + 1) l)
opCodesToInstructions (0x0F:is) = Return : opCodesToInstructions is
opCodesToInstructions (0x0D:i1:i2:is) = JumpIf (fromIntegral i1 + fromIntegral i2 * 256) : opCodesToInstructions is
opCodesToInstructions (0x0C:i1:i2:is) = Jump (fromIntegral i1 + fromIntegral i2 * 256) : opCodesToInstructions is
opCodesToInstructions (0x0B:is) = LessEq : opCodesToInstructions is
opCodesToInstructions (0x0A:is) = Less : opCodesToInstructions is
opCodesToInstructions (0x09:is) = NEq : opCodesToInstructions is
opCodesToInstructions (0x08:is) = Eq : opCodesToInstructions is
opCodesToInstructions (0x07:is) = Div : opCodesToInstructions is
opCodesToInstructions (0x06:is) = Mul : opCodesToInstructions is
opCodesToInstructions (0x05:is) = Sub : opCodesToInstructions is
opCodesToInstructions (0x04:is) = Add : opCodesToInstructions is
opCodesToInstructions (0x03:n:ns) = Get name : opCodesToInstructions (drop (length name) ns)
                                    where
                                        name = getName (n:ns)
opCodesToInstructions (0x01:i1:i2:is) = PushInt (fromIntegral i1 + fromIntegral i2 * 256) : opCodesToInstructions is
opCodesToInstructions (0x15:is) = Print : opCodesToInstructions is
opCodesToInstructions (0x16:ns) = CallFunc name (getParamsInstruction (params)) : opCodesToInstructions rest
                                    where
                                        name = getName (ns)
                                        int1 = fromIntegral (head (drop ((length name) + 1) ns))
                                        int2 = fromIntegral (head (drop ((length name) + 2) ns))
                                        params = take (int1 + int2 * 256) (drop (length name + 3) ns)
                                        rest = drop (length name + 3 + int1 + int2 * 256) ns
opCodesToInstructions d = trace ("\n" ++ (show d)) []


getBasicBlocks :: String -> [BasicBlock] -> Maybe BasicBlock
getBasicBlocks s [] = Nothing
getBasicBlocks s (b:bs) | s == name b = Just b
                        | otherwise = getBasicBlocks s bs

addParams :: [[Instruction]] -> [BasicBlock] -> [Word8]
addParams [] _ = []
addParams (p:ps) b = case p of
    [] -> []
    i -> case instructionsToOpCode2 i b of
        Left l -> l ++ [0xFF] ++ addParams ps b
        Right err -> []

instructionsToOpCode2 :: [Instruction] -> [BasicBlock] -> Either [Word8] String
instructionsToOpCode2 [] _ = Left []
instructionsToOpCode2 ((Set n v):is) b =
  case v of
    IntValue i -> case instructionsToOpCode2 is b of
      Left rest -> Left ([0x0E] ++ stringToBytes n ++ intToBytes i ++ rest)
      Right err -> Right err
    PopValue -> case instructionsToOpCode2 is b of
      Left rest -> Left ([0x0E] ++ stringToBytes n ++ [0x0F] ++ rest)
      Right err -> Right err
instructionsToOpCode2 ((CallFunc "print" p):is) b =
  case length p of
    1 -> case instructionsToOpCode2 (head p) b of
      Left rest -> case instructionsToOpCode2 is b of
        Left rest2 -> Left (rest ++ [0x15] ++ rest2)
        Right err -> Right err
      Right err -> Right err
    _ -> Right "Wrong number of parameters"
-- instructionsToOpCode2 ((CallFunc n p):is) b =
--   case getBasicBlocks n b of
--     Just bb
--       | length p == length (params bb) -> case addParams p b (params bb) of
--           l -> case instructionsToOpCode2 (instructions bb) b of
--             Left bbCode -> case instructionsToOpCode2 is b of
--               Left rest -> Left (l ++ bbCode ++ rest)
--               Right err -> Right err
--             Right err -> Right err
--       | otherwise -> Right "Wrong number of parameters"
--     Nothing -> Right "Function not found"
instructionsToOpCode2 ((CallFunc n p):is) b =
  case getBasicBlocks n b of
    Just bb
      | length p == length (params bb) -> case addParams p b of
          l -> case instructionsToOpCode2 is b of
            Left rest -> Left ([0x16] ++ stringToBytes n ++ intToBytes (length l) ++ l ++ rest)
            Right err -> Right err
      | otherwise -> Right "Wrong number of parameters"
    Nothing -> Right "Function not found"
instructionsToOpCode2 (i:is) b =
  case instructionToOpCode i of
    [] -> instructionsToOpCode2 is b
    l -> case instructionsToOpCode2 is b of
      Left rest -> Left (l ++ rest)
      Right err -> Right err

