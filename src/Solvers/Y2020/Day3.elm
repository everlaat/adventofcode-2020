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
    List (Array Area)


inputToMatrix : String -> Matrix
inputToMatrix =
    String.trim
        >> String.lines
        >> List.map
            (String.toList
                >> List.filterMap
                    (\a ->
                        case a of
                            '#' ->
                                Just Tree

                            '.' ->
                                Just Clear

                            _ ->
                                Nothing
                    )
                >> Array.fromList
            )


partToSolver : (Matrix -> String) -> (String -> Result String String)
partToSolver f =
    inputToMatrix >> f >> Ok


part1 : Matrix -> String
part1 matrix =
    getTreesOnPath matrix ( 3, 1 )
        |> String.fromInt


part2 : Matrix -> String
part2 matrix =
    [ ( 1, 1 )
    , ( 3, 1 )
    , ( 5, 1 )
    , ( 7, 1 )
    , ( 1, 2 )
    ]
        |> List.map (getTreesOnPath matrix)
        |> List.foldl (*) 1
        |> String.fromInt


getTreesOnPath : Matrix -> ( Int, Int ) -> Int
getTreesOnPath matrix ( e, s ) =
    matrix
        |> List.drop s
        |> List.indexedMap Tuple.pair
        |> List.filter (Tuple.first >> (+) 1 >> remainderBy s >> (==) 0)
        |> List.indexedMap (\i ( _, row ) -> Array.get (remainderBy (Array.length row) ((i + 1) * e)) row)
        |> List.filter ((==) (Just Tree))
        |> List.length
