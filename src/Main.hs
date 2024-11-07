{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Use fromMaybe" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}

import Data.Bits (shiftL, testBit)
import Data.ByteString qualified as BS
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.Word (Word8)
import System.Environment (getArgs)


data Instruction = Instruction
    { mnemonic :: String,
      opcodePattern :: [Maybe Int],
      operandHandler :: [[Int]] -> [(String, String, [Int])] -> (String, String, Int)
    }


convertBytesToBitMatrix :: BS.ByteString -> [[Int]]
convertBytesToBitMatrix byteString = map convertByteToBits (BS.unpack byteString)


convertByteToBits :: Word8 -> [Int]
convertByteToBits byte = [if testBit byte j then 1 else 0 | j <- [7, 6 .. 0]]


lookupRegisterPair :: (Eq v) => v -> [(k1, k2, v)] -> Maybe (k1, k2)
lookupRegisterPair searchValue searchTuple = listToMaybe [(k1, k2) | (k1, k2, v) <- searchTuple, v == searchValue]


lookupAddressMode :: [Int] -> String
lookupAddressMode [0, 0, 0] = "bx + si"
lookupAddressMode [0, 0, 1] = "bx + di"
lookupAddressMode [0, 1, 0] = "bp + si"
lookupAddressMode [0, 1, 1] = "bp + di"
lookupAddressMode [1, 0, 0] = "si"
lookupAddressMode [1, 0, 1] = "di"
lookupAddressMode [1, 1, 0] = "bp"
lookupAddressMode [1, 1, 1] = "bx"
lookupAddressMode _ = "err"


matchInstruction :: [Int] -> Instruction -> Bool
matchInstruction bits (Instruction _ pattern _) =
    and $ zipWith matchBit bits pattern
    where
        matchBit bit (Just p) = bit == p
        matchBit _ Nothing = True


handleMovInstruction :: [[Int]] -> [(String, String, [Int])] -> (String, String, Int)
handleMovInstruction bytes registers =
    let
        firstByte = bytes !! 0
        d = firstByte !! 6
        w = firstByte !! 7
        secondByte = bytes !! 1
        modField = take 2 secondByte
        regField = take 3 (drop 2 secondByte)
        rmField = take 3 (drop 5 secondByte)

        -- Determines the register name based on the bit w
        regName regBits = maybe "err" (if w == 0 then fst else snd) (lookupRegisterPair regBits registers)

        -- Handle register-to-register mode when modField is [1,1]
        (source, destination, instrLength) =
            if modField == [1, 1]
                then
                    let
                        regOperand = regName regField
                        rmOperand = regName rmField
                    in
                        if d == 0
                            then (rmOperand, regOperand, 2)
                            else (regOperand, rmOperand, 2)
                else
                    -- Handle memory addressing modes
                    let
                        baseAddress = lookupAddressMode rmField

                        (displacementBits, instrLen) = case modField of
                            [0, 1] -> (bytes !! 2, 3) -- 8-bit displacement
                            [1, 0] -> (bytes !! 2 ++ bytes !! 3, 4) -- 16-bit displacement
                            _ -> ([], 2) -- No displacement
                        displacement = if null displacementBits then 0 else convertDisplacement displacementBits

                        -- Build the displacement string
                        displacementStr
                            | displacement > 0 = " + " ++ show displacement
                            | displacement < 0 = " - " ++ show (abs displacement)
                            | otherwise = ""

                        -- Constructs the final address with displacement inside the brackets
                        address = "[" ++ baseAddress ++ displacementStr ++ "]"

                        regOperand = regName regField
                    in
                        if d == 0
                            then (address, regOperand, instrLen)
                            else (regOperand, address, instrLen)
    in
        (source, destination, instrLength)


handleMovImmediateInstruction :: [[Int]] -> [(String, String, [Int])] -> (String, String, Int)
handleMovImmediateInstruction bytes registers =
    let
        firstByte = bytes !! 0
        w = firstByte !! 4 -- Bit `w` de largura
        regBits = take 3 (drop 5 firstByte)
        regName =
            if w == 0
                then maybe "err" fst (lookupRegisterPair regBits registers)
                else maybe "err" snd (lookupRegisterPair regBits registers)
        immediateValue =
            if w == 0
                then show $ convertBitsToByte (bytes !! 1)
                else show $ convertBitsToByte (bytes !! 1) + (convertBitsToByte (bytes !! 2) `shiftL` 8)
        instrLength = if w == 0 then 2 else 3
    in
        (regName, immediateValue, instrLength)


-- Atualização da função processInstructions
processInstructions :: [[Int]] -> [Instruction] -> [(String, String, [Int])] -> FilePath -> IO ()
processInstructions [] _ _ _ = return ()
processInstructions bytes instructions registers filePath = do
    let
        firstByte = bytes !! 0
    let
        matchedInstruction = find (matchInstruction (take 8 firstByte)) instructions
    case matchedInstruction of
        Just instr -> do
            let
                (destination, source, instrLength) = operandHandler instr bytes registers
            let
                disassembled = mnemonic instr ++ " " ++ destination ++ ", " ++ source
            appendFile filePath (disassembled ++ "\n")
            processInstructions (drop instrLength bytes) instructions registers filePath
        Nothing -> do
            appendFile filePath "Unknown Instruction\n"
            processInstructions (tail bytes) instructions registers filePath


convertBitsToByte :: [Int] -> Int
convertBitsToByte = foldl (\acc b -> acc * 2 + b) 0


convertDisplacement :: [Int] -> Int
convertDisplacement bits =
    let
        value = foldl (\acc b -> acc * 2 + b) 0 bits
    in
        if length bits == 8 && testBit value 7 -- Verifica se é um deslocamento de 8 bits negativo
            then value - 256
            else value


main :: IO ()
main = do
    args <- getArgs

    case args of
        [inputFile, outputFile] -> do
            contents <- BS.readFile inputFile

            let
                instructions =
                    [ Instruction
                        "mov"
                        [Just 1, Just 0, Just 0, Just 0, Just 1, Just 0, Nothing, Nothing]
                        handleMovInstruction,
                      Instruction
                        "mov"
                        [Just 1, Just 0, Just 1, Just 1, Nothing, Nothing, Nothing, Nothing]
                        handleMovImmediateInstruction
                    ]

            let
                registers =
                    [ ("al", "ax", [0, 0, 0] :: [Int]),
                      ("cl", "cx", [0, 0, 1] :: [Int]),
                      ("dl", "dx", [0, 1, 0] :: [Int]),
                      ("bl", "bx", [0, 1, 1] :: [Int]),
                      ("ah", "sp", [1, 0, 0] :: [Int]),
                      ("ch", "bp", [1, 0, 1] :: [Int]),
                      ("dh", "si", [1, 1, 0] :: [Int]),
                      ("bh", "di", [1, 1, 1] :: [Int])
                    ]

            let
                bitMatrix = convertBytesToBitMatrix contents

            writeFile outputFile "bits 16\n\n"

            processInstructions bitMatrix instructions registers outputFile