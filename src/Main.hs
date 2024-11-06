{-# LANGUAGE ImportQualifiedPost #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}
{-# OPTIONS_GHC -Wno-overlapping-patterns #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use head" #-}
{-# HLINT ignore "Use fromMaybe" #-}
{-# HLINT ignore "Replace case with fromMaybe" #-}

import Data.Bits (testBit)
import Data.ByteString qualified as BS
import Data.List (find)
import Data.Maybe (listToMaybe)
import Data.String.Interpolate (i)
import Data.Word (Word8)
import System.Environment (getArgs)


-- Definição da estrutura Instruction
data Instruction = Instruction
    { mnemonic :: String,
      opcodePattern :: [Maybe Int],
      operandHandler :: [[Int]] -> [(String, String, [Int])] -> (String, String)
    }


-- Função para corresponder instruções
matchInstruction :: [Int] -> Instruction -> Bool
matchInstruction bits (Instruction _ pattern _) =
    and $ zipWith matchBit bits pattern
    where
        matchBit bit (Just p) = bit == p
        matchBit _ Nothing = True


-- Manipulador de operandos para a instrução 'mov'
handleMovInstruction :: [[Int]] -> [(String, String, [Int])] -> (String, String)
handleMovInstruction bytes registers =
    let
        firstByte = bytes !! 0
        d = firstByte !! 6
        w = firstByte !! 7
        secondByte = bytes !! 1
        modField = take 2 secondByte
        regField = take 3 (drop 2 secondByte)
        rmField = take 3 (drop 5 secondByte)
        regName regBits = maybe "err" (if w == 0 then fst else snd) (lookupRegisterPair regBits registers)
        source = regName (if d == 0 then rmField else regField)
        destination = regName (if d == 0 then regField else rmField)
    in
        (source, destination)


convertBytesToBitMatrix :: BS.ByteString -> [[Int]]
convertBytesToBitMatrix byteString = map convertByteToBits (BS.unpack byteString)


convertByteToBits :: Word8 -> [Int]
convertByteToBits byte = [if testBit byte j then 1 else 0 | j <- [7, 6 .. 0]]


lookupInstruction :: (Eq v) => v -> [(k, v)] -> Maybe k
lookupInstruction searchValue searchTuple = listToMaybe [k | (k, v) <- searchTuple, v == searchValue]


lookupRegisterPair :: (Eq v) => v -> [(k1, k2, v)] -> Maybe (k1, k2)
lookupRegisterPair searchValue searchTuple = listToMaybe [(k1, k2) | (k1, k2, v) <- searchTuple, v == searchValue]


resolveDestinationAndSource :: [Int] -> [Int] -> [Int] -> [(String, String, [Int])] -> String
resolveDestinationAndSource operation direction registerList registerName =
    case (operation, direction) of
        ([0], [0]) -> maybe "err" fst (lookupRegisterPair registerList registerName)
        ([0], [1]) -> maybe "err" snd (lookupRegisterPair registerList registerName)
        ([1], [0]) -> maybe "err" snd (lookupRegisterPair registerList registerName)
        ([1], [1]) -> maybe "err" fst (lookupRegisterPair registerList registerName)


resolveDirectDestinationAndSource :: [Int] -> [Int] -> [(String, String, [Int])] -> String
resolveDirectDestinationAndSource operation registerList registerName =
    case operation of
        [0] -> maybe "err" fst (lookupRegisterPair registerList registerName)
        [1] -> maybe "err" snd (lookupRegisterPair registerList registerName)


convertBinaryToDecimal :: [Int] -> String
convertBinaryToDecimal = show . foldl (\acc b -> acc * 2 + b) 0


extractByteValue :: [[Int]] -> Int -> String
extractByteValue bytes j =
    let
        byte = bytes !! j
    in
        convertBinaryToDecimal byte


extractDoubleByteValue :: [[Int]] -> Int -> Int -> String
extractDoubleByteValue bytes k l =
    let
        byte1 = bytes !! k
        byte2 = bytes !! l
        combinedBytes = byte1 ++ byte2
    in
        convertBinaryToDecimal combinedBytes


processInstruction :: [[Int]] -> (String, String, String) -> Maybe String
processInstruction bytes (mnemonic, source, destination)
    | length bytes `elem` [2, 3, 4] = Just [i|#{mnemonic} #{source}, #{destination}|]
    | null bytes = Just "err"
    | otherwise = Nothing


-- processInstructions
--     :: [[Int]]
--     -> ([(String, [Int])], [(String, String, [Int])])
--     -> FilePath
--     -> IO ()
-- processInstructions [] _ _ = return ()
-- processInstructions [_] _ _ = return ()
-- processInstructions bytes context filePath = do
--     let
--         firstByte = bytes !! 0

--         (instructions, registers) = context

--         maybeInstruction4Bits = lookupInstruction (take 4 firstByte) instructions
--         maybeInstruction6Bits = lookupInstruction (take 6 firstByte) instructions

--     (split, operation, source, destination) <-
--         case (maybeInstruction4Bits, maybeInstruction6Bits) of
--             -- Direct Addess Field Encoding
--             (Just operation, Nothing) -> do
--                 let
--                     instructionW = take 1 (drop 4 firstByte)
--                     extensionREG = take 3 (drop 5 firstByte)
--                     source = resolveDirectDestinationAndSource instructionW extensionREG registers

--                 let
--                     splitW = case instructionW of
--                         [0] -> 2
--                         [1] -> 3

--                 let
--                     destination =
--                         if splitW == 2
--                             then
--                                 extractByteValue bytes 1
--                             else
--                                 extractDoubleByteValue bytes 1 2
--                 return (splitW, operation, source, destination)
--             (Nothing, Just operation) -> do
--                 let
--                     secondByte = bytes !! 1

--                     directionD = take 1 (drop 6 firstByte)
--                     instructionW = take 1 (drop 7 firstByte)

--                     -- modeMOD = take 2 secondByte
--                     extensionREG = take 3 (drop 2 secondByte)
--                     registerRM = take 3 (drop 5 secondByte)

--                     source = resolveDestinationAndSource instructionW directionD registerRM registers
--                     destination = resolveDestinationAndSource instructionW directionD extensionREG registers

--                 -- let
--                 --     splitMOD = case modeMOD of
--                 --         [0, 0] -> 2
--                 --         [0, 1] -> 2
--                 --         [1, 0] -> 2
--                 --         [1, 1] -> 2

--                 return (2, operation, source, destination)
--             (Nothing, Nothing) -> do
--                 return (0, "err", "err", "err")

--     let
--         (currentBytes, restOfBytes) = splitAt split bytes

--     let
--         result = processInstruction currentBytes (operation, source, destination)

--     case result of
--         Just res -> appendFile filePath (res ++ "\n")
--         Nothing -> appendFile filePath "no match found\n"

--     processInstructions restOfBytes context filePath

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
                (source, destination) = operandHandler instr bytes registers
            let
                disassembled = mnemonic instr ++ " " ++ destination ++ ", " ++ source
            appendFile filePath (disassembled ++ "\n")
            -- Determine the length of the instruction (e.g., 2 bytes for 'mov' with ModRM)
            let
                instrLength = 2 -- Ajuste conforme necessário
            processInstructions (drop instrLength bytes) instructions registers filePath
        Nothing -> do
            appendFile filePath "Unknown instruction\n"
            processInstructions (tail bytes) instructions registers filePath


main :: IO ()
main = do
    args <- getArgs

    case args of
        [inputFile, outputFile] -> do
            contents <- BS.readFile inputFile

            -- let
            --     instructions =
            --         [ ("vom", [1, 0, 1, 1] :: [Int]),
            --           ("mov", [1, 0, 0, 0, 1, 0] :: [Int])
            --         ]

            -- Defina as instruções usando o tipo `Instruction`
            let
                instructions =
                    [ Instruction "mov" [Just 1, Just 0, Just 0, Just 0, Just 1, Just 0, Nothing, Nothing] handleMovInstruction
                    -- Adicione outras instruções conforme necessário
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
            -- processInstructions bitMatrix (instructions, registers) outputFile

        _ -> putStrLn "Uso: sim8086 <arquivo_entrada> <arquivo_saida>"
