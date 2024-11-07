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


-- | Define o tipo de dados 'Instruction', que representa uma instrução de montagem.
data Instruction = Instruction
    { mnemonic :: String,
      opcode :: [Maybe Int],
      operand :: [[Int]] -> [(String, String, [Int])] -> [(String, [Int])] -> (String, String, Int)
    }


-- | Converte uma lista de bits em um valor inteiro correspondente.
convertBitsToByte :: [Int] -> Int
convertBitsToByte = foldl (\acc bit -> acc * 2 + bit) 0


-- | Converte um byte (Word8) em uma lista de bits, do bit mais significativo ao menos significativo.
convertByteToBits :: Word8 -> [Int]
convertByteToBits byte = [if testBit byte i then 1 else 0 | i <- [7, 6 .. 0]]


-- | Converte um ByteString em uma matriz de bits, onde cada linha representa os bits de um byte.
convertBytesToBitMatrix :: BS.ByteString -> [[Int]]
convertBytesToBitMatrix byteString = map convertByteToBits (BS.unpack byteString)


-- | Procura um par de registros correspondente ao valor fornecido na lista de tuplas.
lookupRegisterPair :: (Eq value) => value -> [(regLow, regHigh, value)] -> Maybe (regLow, regHigh)
lookupRegisterPair value registerPairs = listToMaybe [(regLow, regHigh) | (regLow, regHigh, v) <- registerPairs, v == value]


-- | Procura o modo de endereçamento correspondente aos bits fornecidos na lista de modos.
lookupAddressMode :: [(String, [Int])] -> [Int] -> String
lookupAddressMode addressModes bits = maybe "err" fst (find (\(_, modeBits) -> modeBits == bits) addressModes)


-- | Verifica se a lista de bits corresponde ao padrão da instrução
findInstruction :: [Int] -> Instruction -> Bool
findInstruction bits (Instruction _ pattern _) =
    and (zipWith matchBit bits pattern)
    where
        matchBit bit (Just target) = bit == target
        matchBit _ Nothing = True


-- | Converte uma lista de bits em um deslocamento, tratando o bit mais significativo como sinal quando há 8 bits.
convertDisplacement :: [Int] -> Int
convertDisplacement bits =
    let
        value = convertBitsToByte bits
    in
        if length bits == 8 && testBit value 7
            then value - 256
            else value


-- | Decodifica uma instrução 'mov' que envolve registros e modos de endereçamento.
handleMovInstruction :: [[Int]] -> [(String, String, [Int])] -> [(String, [Int])] -> (String, String, Int)
handleMovInstruction bytes registers modes =
    let
        firstByte = bytes !! 0
        -- Obtém o bit 'd', que indica a direção da transferência (0 para destino na memória, 1 para destino no registrador).
        dField = firstByte !! 6
        -- Obtém o bit 'w', que indica o tamanho do valor (0 para 8 bits, 1 para 16 bits).
        wField = firstByte !! 7

        secondByte = bytes !! 1
        modField = take 2 secondByte
        regField = take 3 (drop 2 secondByte)
        rmField = take 3 (drop 5 secondByte)

        -- Obtém o nome do registro de acordo com os bits e o valor de 'wField'.
        regName regBits = maybe "err" (if wField == 0 then fst else snd) (lookupRegisterPair regBits registers)

        (source, destination, size) =
            -- Caso modField seja [1, 1], ambos os operandos são registradores.
            if modField == [1, 1]
                then
                    let
                        regOperand = regName regField
                        rmOperand = regName rmField
                    in
                        if dField == 0
                            -- Se 'dField' for 0, 'rmOperand' é a origem e 'regOperand' é o destino.
                            then (rmOperand, regOperand, 2)
                            -- Caso contrário, 'regOperand' é a origem e 'rmOperand' é o destino.
                            else (regOperand, rmOperand, 2)
                else
                    let
                        -- Procura o modo de endereçamento correspondente aos bits 'rmField'.
                        baseAddress = lookupAddressMode modes rmField

                        (displacementBits, instructionSize) =
                            case modField of
                                -- Deslocamento de 8 bits.
                                [0, 1] -> (bytes !! 2, 3)
                                -- Deslocamento de 16 bits.
                                [1, 0] -> (bytes !! 2 ++ bytes !! 3, 4)
                                -- Sem deslocamento.
                                _ -> ([], 2)

                        -- Converte o deslocamento, se houver.
                        displacement = if null displacementBits then 0 else convertDisplacement displacementBits

                        displacementString
                            | displacement > 0 = " + " ++ show displacement
                            | displacement < 0 = " - " ++ show (abs displacement)
                            | otherwise = ""

                        -- Monta o endereço completo incluindo o deslocamento, se houver.
                        address = "[" ++ baseAddress ++ displacementString ++ "]"

                        regOperand = regName regField
                    in
                        if dField == 0
                            -- Se 'dField' for 0, o endereço é a origem e o registrador é o destino.
                            then (address, regOperand, instructionSize)
                            -- Caso contrário, o registrador é a origem e o endereço é o destino.
                            else (regOperand, address, instructionSize)
    in
        (source, destination, size)


-- | Lida com a instrução 'mov' imediata, onde o valor a ser movido está embutido na própria instrução.
handleMovImmediateInstruction ::
    [[Int]] -> [(String, String, [Int])] -> [(String, [Int])] -> (String, String, Int)
handleMovImmediateInstruction bytes registers _ =
    let
        firstByte = bytes !! 0
        wField = firstByte !! 4
        -- Extrai os três bits do campo de registro, que indicam o registro de destino.
        regBits = take 3 (drop 5 firstByte)

        regName =
            -- Determina o nome do registro de destino, considerando registros de 8 bits se 'wField' for 0 e 16 bits se 'wField' for 1.
            if wField == 0
                then maybe "err" fst (lookupRegisterPair regBits registers)
                else maybe "err" snd (lookupRegisterPair regBits registers)

        immediateValue =
            if wField == 0
                -- Obtém o valor imediato de 8 bits a ser movido.
                then show $ convertBitsToByte (bytes !! 1)
                -- Obtém o valor imediato de 16 bits a ser movido, combinando dois bytes.
                else show $ convertBitsToByte (bytes !! 1) + (convertBitsToByte (bytes !! 2) `shiftL` 8)

        -- Define o tamanho da instrução, 2 bytes para valores de 8 bits e 3 bytes para valores de 16 bits.
        instructionSize = if wField == 0 then 2 else 3
    in
        (regName, immediateValue, instructionSize)


-- | Processa uma lista de bytes (representados como listas de bits) e escreve as instruções desmontadas em um arquivo de saída.
processInstructions ::
    [[Int]] -> [Instruction] -> [(String, String, [Int])] -> [(String, [Int])] -> FilePath -> IO ()
processInstructions [] _ _ _ _ = return ()
processInstructions bytes instructions registers address filePath = do
    let
        firstByte = bytes !! 0

    let
        -- Tenta encontrar uma instrução que corresponda ao padrão dos bits do primeiro byte.
        matchedInstruction = find (findInstruction (take 8 firstByte)) instructions

    case matchedInstruction of
        Just instruction -> do
            let
                -- Usa a função 'operand' da instrução para interpretar os operandos (destino, origem) e determinar o comprimento da instrução.
                (destination, source, size) = operand instruction bytes registers address

            let
                -- Monta a instrução desmontada em formato de texto, incluindo o mnemônico e os operandos.
                disassembled = mnemonic instruction ++ " " ++ destination ++ ", " ++ source

            appendFile filePath (disassembled ++ "\n")
            processInstructions (drop size bytes) instructions registers address filePath
        Nothing -> do
            appendFile filePath "err\n"
            processInstructions (tail bytes) instructions registers address filePath


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
                address =
                    [ ("bx + si", [0, 0, 0] :: [Int]),
                      ("bx + di", [0, 0, 1] :: [Int]),
                      ("bp + si", [0, 1, 0] :: [Int]),
                      ("bp + di", [0, 1, 1] :: [Int]),
                      ("si", [1, 0, 0] :: [Int]),
                      ("di", [1, 0, 1] :: [Int]),
                      ("bp", [1, 1, 0] :: [Int]),
                      ("bx", [1, 1, 1] :: [Int])
                    ]

            let
                bitMatrix = convertBytesToBitMatrix contents

            writeFile outputFile "bits 16\n\n"

            processInstructions bitMatrix instructions registers address outputFile