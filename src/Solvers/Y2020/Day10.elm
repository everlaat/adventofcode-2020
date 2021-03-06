module Solvers.Y2020.Day10 exposing (part1, part2, partToSolver, solvers)

import Com.Solver as Solver exposing (Solver)
import Dict
import Dict.Extra as Dict
import List
import Maybe
import Maybe.Extra as Maybe


solvers : List Solver
solvers =
    [ Solver.make 2020 10 1 (part1 |> partToSolver)
    , Solver.make 2020 10 2 (part2 |> partToSolver)
    ]


partToSolver : (List Int -> Maybe Int) -> (String -> Result String String)
partToSolver f =
    inputToSortedListInt
        >> f
        >> Maybe.map String.fromInt
        >> Result.fromMaybe "answer not found"


inputToSortedListInt : String -> List Int
inputToSortedListInt =
    String.trim
        >> String.lines
        >> List.filterMap String.toInt
        >> List.sort


part1 : List Int -> Maybe Int
part1 list =
    let
        dictFrequencies =
            List.map2 (-) list (0 :: list)
                |> Dict.frequencies
    in
    Maybe.map2 (\a b -> a * (b + 1))
        (Dict.get 1 dictFrequencies)
        (Dict.get 3 dictFrequencies)


part2 : List Int -> Maybe Int
part2 =
    List.foldl
        (\a r ->
            List.foldl
                (\b r_ ->
                    if a - Tuple.first b <= 3 then
                        b :: r_

                    else
                        r_
                )
                []
                r
                |> List.map Tuple.second
                |> List.sum
                |> Tuple.pair a
                |> (\c -> c :: r)
        )
        [ ( 0, 1 ) ]
        >> List.head
        >> Maybe.map Tuple.second
