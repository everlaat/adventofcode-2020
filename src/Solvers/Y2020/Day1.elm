module Solvers.Y2020.Day1 exposing (part1, part2, solvers)

import Com.Solver as Solver exposing (Solver)


solvers : List Solver
solvers =
    [ Solver.make 2020 1 1 (part1 2020 |> partToSolver)
    , Solver.make 2020 1 2 (part1 2020 |> partToSolver)
    ]


partToSolver : (String -> Maybe Int) -> (String -> Result String String)
partToSolver f =
    f >> (Maybe.map (String.fromInt >> Ok) >> Maybe.withDefault (Err "Target not found"))


part1 : Int -> String -> Maybe Int
part1 =
    findTargetThroughAdditionAndMultiplyResult listMakePairs uncurry


part2 : Int -> String -> Maybe Int
part2 =
    findTargetThroughAdditionAndMultiplyResult listMakeTrios uncurry3


findTargetThroughAdditionAndMultiplyResult : (List Int -> List b) -> ((Int -> Int -> Int) -> b -> Int) -> Int -> String -> Maybe Int
findTargetThroughAdditionAndMultiplyResult fa fb target =
    inputToList
        >> fa
        >> List.filter (fb (+) >> (==) target)
        >> List.head
        >> Maybe.map (fb (*))


inputToList : String -> List Int
inputToList =
    String.split "\n"
        >> List.filterMap (String.toInt << String.trim)


listMakePairs : List a -> List ( a, a )
listMakePairs xs =
    case xs of
        [] ->
            []

        x :: xs_ ->
            List.map (Tuple.pair x) xs_
                ++ listMakePairs xs_


listMakeTrios : List a -> List ( a, a, a )
listMakeTrios xs =
    case xs of
        [] ->
            []

        x :: xs_ ->
            (listMakePairs xs_
                |> List.map (\( a, b ) -> ( a, b, x ))
            )
                ++ listMakeTrios xs_


uncurry : (Int -> Int -> Int) -> ( Int, Int ) -> Int
uncurry f ( a, b ) =
    f a b


uncurry3 : (Int -> Int -> Int) -> ( Int, Int, Int ) -> Int
uncurry3 f ( a, b, c ) =
    f (f a b) c
