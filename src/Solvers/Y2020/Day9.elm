module Solvers.Y2020.Day9 exposing (part1, part2, partToSolver, solvers)

import Array
import Com.Solver as Solver exposing (Solver)
import List
import List.Extra as List
import Maybe.Extra as Maybe


solvers : List Solver
solvers =
    [ Solver.make 2020 8 1 (part1 25 |> partToSolver)
    , Solver.make 2020 8 2 (part2 25 |> partToSolver)
    ]


partToSolver : (List Int -> Maybe Int) -> (String -> Result String String)
partToSolver f =
    inputToNumbers
        >> f
        >> Maybe.map String.fromInt
        >> Result.fromMaybe "answer not found"


inputToNumbers : String -> List Int
inputToNumbers =
    String.trim
        >> String.lines
        >> List.filterMap String.toInt


part1 : Int -> List Int -> Maybe Int
part1 preamble numbers =
    let
        arr =
            Array.fromList numbers

        isTheSumOfTwo index =
            case
                Array.slice (index - preamble - 1) index arr
                    |> Array.toList
                    |> List.reverse
            of
                target :: rest ->
                    List.uniquePairs rest
                        |> List.map (\( a, b ) -> a + b)
                        |> List.member target

                _ ->
                    False
    in
    List.range (preamble + 1) (List.length numbers)
        |> List.foldl
            (\index invalid ->
                if isTheSumOfTwo index then
                    invalid

                else
                    Array.get (index - 1) arr :: invalid
            )
            []
        |> List.filterMap identity
        |> List.head


part2 : Int -> List Int -> Maybe Int
part2 preamble numbers =
    let
        invalidNumber =
            part1 preamble numbers
                |> Maybe.withDefault 0

        arr =
            Array.fromList numbers
    in
    List.range 0 (List.length numbers)
        |> List.foldl
            (\i r ->
                if r == Nothing then
                    List.range i (List.length numbers)
                        |> List.foldl
                            (\i_ r_ ->
                                let
                                    window =
                                        Array.slice i i_ arr
                                            |> Array.toList

                                    sum =
                                        List.foldl (+) 0 window
                                in
                                if r_ == Nothing && sum == invalidNumber then
                                    Maybe.map2 (+)
                                        (List.minimum window)
                                        (List.maximum window)

                                else
                                    r_
                            )
                            r

                else
                    r
            )
            Nothing
