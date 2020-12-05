module Solvers.Y2020.Day5 exposing (part1, part2, partToSolver, solvers)

import Binary
import Com.Solver as Solver exposing (Solver)


solvers : List Solver
solvers =
    [ Solver.make 2020 5 1 (part1 |> partToSolver)
    , Solver.make 2020 5 2 (part2 |> partToSolver)
    ]


partToSolver : (List Int -> Maybe Int) -> (String -> Result String String)
partToSolver f =
    inputToListOfInt
        >> f
        >> Maybe.map String.fromInt
        >> Result.fromMaybe "answer not found"


inputToListOfInt : String -> List Int
inputToListOfInt =
    String.trim
        >> String.lines
        >> List.map (String.toList >> parsePass)


parsePass : List Char -> Int
parsePass =
    List.foldl
        (\operator ( r, c ) ->
            case operator of
                'B' ->
                    ( 1 :: r, c )

                'F' ->
                    ( 0 :: r, c )

                'L' ->
                    ( r, 0 :: c )

                'R' ->
                    ( r, 1 :: c )

                _ ->
                    ( r, c )
        )
        ( [], [] )
        >> (\( a, b ) ->
                let
                    parseBits =
                        List.reverse
                            >> Binary.fromIntegers
                            >> Binary.toDecimal
                in
                ( parseBits a
                , parseBits b
                )
           )
        >> (\( r, c ) -> (r * 8) + c)


part1 : List Int -> Maybe Int
part1 =
    List.maximum


part2 : List Int -> Maybe Int
part2 list =
    Maybe.andThen
        (foldMissing (List.sort list |> List.reverse))
        (part1 list)


foldMissing : List Int -> Int -> Maybe Int
foldMissing list a =
    case list of
        [] ->
            Nothing

        xs :: rest ->
            if xs == a then
                foldMissing rest (a - 1)

            else
                Just a
