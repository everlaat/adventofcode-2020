module Solvers.Y2020.Day9 exposing (part1, part2, partToSolver, solvers)

import Array exposing (Array)
import Com.Solver as Solver exposing (Solver)
import List
import List.Extra as List
import Maybe.Extra as Maybe
import Tuple.Extra as Tuple


solvers : List Solver
solvers =
    [ Solver.make 2020 8 1 (part1 25 |> partToSolver)
    , Solver.make 2020 8 2 (part2 25 |> partToSolver)
    ]


partToSolver : (Array Int -> Maybe Int) -> (String -> Result String String)
partToSolver f =
    inputToNumbers
        >> f
        >> Maybe.map String.fromInt
        >> Result.fromMaybe "answer not found"


inputToNumbers : String -> Array Int
inputToNumbers =
    String.trim
        >> String.lines
        >> List.filterMap String.toInt
        >> Array.fromList


part1 : Int -> Array Int -> Maybe Int
part1 preamble numbers =
    let
        isTheSumOfTwo index =
            case
                Array.slice (index - preamble - 1) index numbers
                    |> Array.toList
                    |> List.reverse
            of
                target :: rest ->
                    List.uniquePairs rest
                        |> List.map (Tuple.apply (+))
                        |> List.member target

                _ ->
                    False
    in
    List.range (preamble + 1) (Array.length numbers)
        |> List.foldl
            (\index invalid ->
                if isTheSumOfTwo index then
                    invalid

                else
                    Array.get (index - 1) numbers :: invalid
            )
            []
        |> List.filterMap identity
        |> List.head


part2 : Int -> Array Int -> Maybe Int
part2 preamble numbers =
    let
        invalidNumber =
            part1 preamble numbers
                |> Maybe.withDefault 0
    in
    List.range 0 (Array.length numbers)
        |> List.foldl
            (\i r ->
                if r == Nothing then
                    Array.length numbers
                        |> List.range (i + 2)
                        |> List.foldl
                            (\i_ r_ ->
                                if r_ == Nothing then
                                    let
                                        window =
                                            Array.slice i i_ numbers
                                                |> Array.toList

                                        sum =
                                            List.foldl (+) 0 window
                                    in
                                    if sum == invalidNumber then
                                        Maybe.map2 (+)
                                            (List.minimum window)
                                            (List.maximum window)

                                    else
                                        Nothing

                                else
                                    r_
                            )
                            Nothing

                else
                    r
            )
            Nothing
