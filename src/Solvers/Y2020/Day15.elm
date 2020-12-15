module Solvers.Y2020.Day15 exposing (part1, part2, partToSolver, solvers)

import Array
import Com.Solver as Solver exposing (Solver)
import Dict exposing (Dict)
import List
import List.Extra as List
import Maybe.Extra as Maybe


solvers : List Solver
solvers =
    [ Solver.make 2020 15 1 (part1 |> partToSolver)
    , Solver.make 2020 15 2 (part2 |> partToSolver)
    ]


partToSolver : (Numbers -> Int) -> (String -> Result String String)
partToSolver f =
    inputToNumbers
        >> f
        >> String.fromInt
        >> Ok


type alias Numbers =
    List Int


inputToNumbers : String -> List Int
inputToNumbers =
    String.trim >> String.split "," >> List.filterMap String.toInt


part1 : Numbers -> Int
part1 =
    recite 2020


part2 : Numbers -> Int
part2 =
    recite 30000000


recite : Int -> Numbers -> Int
recite amount numbers =
    let
        indices =
            numbers
                |> List.indexedFoldl
                    (\index number r ->
                        if index < (List.length numbers - 1) then
                            ( number, index ) :: r

                        else
                            r
                    )
                    []
                |> Dict.fromList
    in
    List.range (List.length numbers) (amount - 1)
        |> List.foldl
            (\index ( arr, ind ) ->
                case Array.get (index - 1) arr of
                    Nothing ->
                        ( arr, ind )

                    Just lastNumber ->
                        case Dict.get lastNumber ind of
                            Nothing ->
                                ( Array.push 0 arr
                                , Dict.insert lastNumber (index - 1) ind
                                )

                            Just lastSeen ->
                                ( Array.push (index - lastSeen - 1) arr
                                , Dict.insert lastNumber (index - 1) ind
                                )
            )
            ( Array.fromList numbers, indices )
        |> Tuple.first
        |> Array.toList
        |> List.reverse
        |> List.head
        |> Maybe.withDefault 0
