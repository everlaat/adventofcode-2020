module Solvers.Y2020.Day3 exposing (part1, part2, partToSolver, solvers)

import Array exposing (Array)
import Com.Solver as Solver exposing (Solver)


solvers : List Solver
solvers =
    [ Solver.make 2020 3 1 (part1 |> partToSolver)
    , Solver.make 2020 3 2 (part2 |> partToSolver)
    ]


type Area
    = Clear
    | Tree


type alias Matrix =
    Array (Array Area)


inputToMatrix : String -> Matrix
inputToMatrix =
    String.trim
        >> String.lines
        >> List.map
            (String.split ""
                >> List.map
                    (\a ->
                        if a == "." then
                            Clear

                        else
                            Tree
                    )
                >> Array.fromList
            )
        >> Array.fromList


partToSolver : (Matrix -> String) -> (String -> Result String String)
partToSolver f =
    inputToMatrix
        >> f
        >> Ok


part1 : Matrix -> String
part1 matrix =
    let
        result =
            List.repeat (Array.length matrix) Clear
    in
    List.indexedMap
        (\index _ ->
            let
                x =
                    (index + 1) * 3

                y =
                    (index + 1) * 1

                row =
                    Array.get y matrix

                cellLength =
                    Maybe.map Array.length row
                        |> Maybe.withDefault 1

                cell =
                    remainderBy cellLength x

                cellContents =
                    row
                        |> Maybe.map (\r -> Array.get cell r)
                        |> Maybe.andThen identity
                        |> Maybe.withDefault Clear
            in
            cellContents
        )
        result
        |> List.filter ((==) Tree)
        |> List.length
        |> String.fromInt


part2 : Matrix -> String
part2 matrix =
    let
        result =
            List.repeat (Array.length matrix) Clear

        moves =
            [ ( 1, 1 ), ( 3, 1 ), ( 5, 1 ), ( 7, 1 ), ( 1, 2 ) ]
    in
    moves
        |> List.map
            (\( mx, my ) ->
                List.indexedMap
                    (\index _ ->
                        let
                            x =
                                (index + 1) * mx

                            y =
                                (index + 1) * my

                            row =
                                Array.get y matrix

                            cellLength =
                                Maybe.map Array.length row
                                    |> Maybe.withDefault 1

                            cell =
                                remainderBy cellLength x

                            cellContents =
                                row
                                    |> Maybe.map (\r -> Array.get cell r)
                                    |> Maybe.andThen identity
                                    |> Maybe.withDefault Clear
                        in
                        cellContents
                    )
                    result
                    |> List.filter ((==) Tree)
                    |> List.length
            )
        |> List.foldl
            (\a b -> a * b)
            1
        |> String.fromInt
