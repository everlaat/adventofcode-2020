module Solvers.Y2020.Day14 exposing (part1, part2, partToSolver, solvers)

import Array exposing (Array)
import Binary
import Com.Solver as Solver exposing (Solver)
import Dict exposing (Dict)
import List
import List.Extra as List
import Maybe.Extra as Maybe
import Parser exposing ((|.), (|=), Parser)
import Parser.Advanced as ParserA


solvers : List Solver
solvers =
    [ Solver.make 2020 14 1 (part1 |> partToSolver)
    , Solver.make 2020 14 2 (part2 |> partToSolver)
    ]


partToSolver : (Program -> Int) -> (String -> Result String String)
partToSolver f =
    parseInput
        >> f
        >> String.fromInt
        >> Ok


type MaskBit
    = X
    | Zero
    | One


type alias Mask =
    List MaskBit


type Instruction
    = UpdateMask Mask
    | WriteValue Int Int


type alias Program =
    { pc : Int
    , mask : Mask
    , instructions : Array Instruction
    , memory : Dict Int Int
    }


part1 : Program -> Int
part1 =
    applyNextInstruction
        >> .memory
        >> Dict.values
        >> List.sum


part2 : Program -> Int
part2 =
    applyNextDecoder
        >> .memory
        >> Dict.values
        >> List.sum


applyNextDecoder : Program -> Program
applyNextDecoder program =
    Array.get program.pc program.instructions
        |> Maybe.unwrap program
            (\i ->
                (case i of
                    UpdateMask mask ->
                        { program | mask = mask, pc = program.pc + 1 }

                    WriteValue addr val ->
                        { program
                            | memory =
                                getAddresses program.mask addr
                                    |> List.foldl
                                        (\a mem ->
                                            Dict.insert a val mem
                                        )
                                        program.memory
                            , pc = program.pc + 1
                        }
                )
                    |> applyNextDecoder
            )


getAddresses : Mask -> Int -> List Int
getAddresses mask val =
    let
        bits =
            val
                |> Binary.fromDecimal
                |> Binary.ensureSize (List.length mask)
                |> Binary.toIntegers
    in
    List.map2
        (\b m ->
            case m of
                Zero ->
                    if b == 0 then
                        Zero

                    else
                        One

                One ->
                    One

                X ->
                    X
        )
        bits
        mask
        |> List.indexedMap Tuple.pair
        |> List.foldl
            (\( i, b ) all ->
                case b of
                    Zero ->
                        all |> List.map (Array.set i 0)

                    One ->
                        all |> List.map (Array.set i 1)

                    X ->
                        all
                            |> List.map
                                (\a ->
                                    [ Array.set i 0 a, Array.set i 1 a ]
                                )
                            |> List.concat
            )
            [ List.repeat 36 0 |> Array.fromList ]
        |> List.map
            (Array.toList
                >> Binary.fromIntegers
                >> Binary.toDecimal
            )


applyNextInstruction : Program -> Program
applyNextInstruction program =
    Array.get program.pc program.instructions
        |> Maybe.unwrap program
            (\i ->
                (case i of
                    UpdateMask mask ->
                        { program | mask = mask, pc = program.pc + 1 }

                    WriteValue addr val ->
                        { program
                            | memory =
                                Dict.insert addr
                                    (applyMask program.mask val)
                                    program.memory
                            , pc = program.pc + 1
                        }
                )
                    |> applyNextInstruction
            )


applyMask : List MaskBit -> Int -> Int
applyMask arr val =
    let
        bits =
            val
                |> Binary.fromDecimal
                |> Binary.ensureSize (List.length arr)
                |> Binary.toIntegers
    in
    List.map2
        (\b m ->
            case m of
                X ->
                    b

                Zero ->
                    0

                One ->
                    1
        )
        bits
        arr
        |> Binary.fromIntegers
        |> Binary.toDecimal


parseInput : String -> Program
parseInput lines =
    { pc = 0
    , memory = Dict.empty
    , mask = List.repeat 36 Zero
    , instructions =
        lines
            |> String.lines
            |> List.filterMap (Parser.run instruction >> Result.toMaybe)
            |> Array.fromList
    }


parseMask : String -> List MaskBit
parseMask s =
    String.toList s
        |> List.filterMap
            (\c ->
                case c of
                    'X' ->
                        Just X

                    '1' ->
                        Just One

                    '0' ->
                        Just Zero

                    _ ->
                        Nothing
            )


instruction : Parser Instruction
instruction =
    Parser.oneOf [ updateMask, writeValue ]


updateMask : Parser Instruction
updateMask =
    Parser.succeed UpdateMask
        |. Parser.keyword "mask"
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Parser.map parseMask (ParserA.chompUntilEndOr "\n" |> Parser.getChompedString)


writeValue : Parser Instruction
writeValue =
    Parser.succeed WriteValue
        |. Parser.keyword "mem"
        |. Parser.symbol "["
        |= Parser.int
        |. Parser.symbol "]"
        |. Parser.spaces
        |. Parser.symbol "="
        |. Parser.spaces
        |= Parser.int
