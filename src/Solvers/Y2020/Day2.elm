module Solvers.Y2020.Day2 exposing (part1, part2, solvers)

import Com.Solver as Solver exposing (Solver)


solvers : List Solver
solvers =
    [ Solver.make 2020 2 1 (part1 |> partToSolver)
    , Solver.make 2020 2 2 (part2 |> partToSolver)
    ]


partToSolver : (String -> String) -> (String -> Result String String)
partToSolver f =
    f >> Ok


inputToList : String -> List String
inputToList =
    String.lines >> List.map String.trim


listToPasswords : List String -> List Password
listToPasswords =
    List.filterMap
        (\a ->
            case String.split " " a of
                [ range, letter, password ] ->
                    case
                        ( String.split "-" range |> List.filterMap String.toInt
                        , String.replace ":" "" letter |> String.uncons
                        )
                    of
                        ( [ x, y ], Just ( char, _ ) ) ->
                            Password x y char password
                                |> Just

                        _ ->
                            Nothing

                _ ->
                    Nothing
        )


part1 : String -> String
part1 =
    inputToList
        >> listToPasswords
        >> List.filter
            (\{ x, y, char, password } ->
                let
                    amount =
                        String.indexes (String.fromChar char) password
                            |> List.length
                in
                amount >= x && amount <= y
            )
        >> List.length
        >> String.fromInt


type alias Password =
    { x : Int
    , y : Int
    , char : Char
    , password : String
    }


part2 : String -> String
part2 =
    inputToList
        >> listToPasswords
        >> List.filter
            (\{ x, y, char, password } ->
                let
                    indexes =
                        String.indexes (String.fromChar char) password
                            |> List.map ((+) 1)

                    a =
                        List.member x indexes

                    b =
                        List.member y indexes
                in
                a && not b || not a && b
            )
        >> List.length
        >> String.fromInt
