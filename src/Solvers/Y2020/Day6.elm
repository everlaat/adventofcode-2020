module Solvers.Y2020.Day6 exposing (part1, part2, partToSolver, solvers)

import Com.Solver as Solver exposing (Solver)
import List
import List.Extra as List
import Maybe.Extra as Maybe
import Set exposing (Set)


solvers : List Solver
solvers =
    [ Solver.make 2020 6 1 (part1 |> partToSolver)
    , Solver.make 2020 6 2 (part2 |> partToSolver)
    ]


partToSolver : (List (List (Set Char)) -> Maybe Int) -> (String -> Result String String)
partToSolver f =
    inputToGroupAnswers
        >> f
        >> Maybe.map String.fromInt
        >> Result.fromMaybe "answer not found"


inputToGroupAnswers : String -> List (List (Set Char))
inputToGroupAnswers =
    String.trim
        >> String.split "\n\n"
        >> List.map (String.lines >> List.map (String.toList >> Set.fromList))


part1 : List (List (Set Char)) -> Maybe Int
part1 =
    List.map (List.foldl1 Set.union)
        >> List.map (Maybe.unwrap 0 Set.size)
        >> List.foldl (+) 0
        >> Just


part2 : List (List (Set Char)) -> Maybe Int
part2 =
    List.map (List.foldl1 Set.intersect)
        >> List.map (Maybe.unwrap 0 Set.size)
        >> List.foldl (+) 0
        >> Just
