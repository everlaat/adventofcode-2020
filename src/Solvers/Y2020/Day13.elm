module Solvers.Y2020.Day13 exposing (part1, part2, partToSolver, solvers)

import Com.Solver as Solver exposing (Solver)
import List
import List.Extra as List
import Maybe.Extra as Maybe


solvers : List Solver
solvers =
    [ Solver.make 2020 13 1 (part1 |> partToSolver)
    , Solver.make 2020 13 2 (part2 |> partToSolver)
    ]


partToSolver : (( Int, List Bus ) -> Int) -> (String -> Result String String)
partToSolver f =
    inputToNotes
        >> f
        >> String.fromInt
        >> Ok


inputToNotes : String -> ( Int, List Bus )
inputToNotes str =
    case String.trim str |> String.lines of
        [ timestamp, ids ] ->
            ( String.toInt timestamp |> Maybe.withDefault 0
            , ids
                |> String.split ","
                |> List.map String.toInt
            )

        _ ->
            ( 0, [] )


type alias Bus =
    Maybe Int


part1 : ( Int, List Bus ) -> Int
part1 ( timestamp, busIds ) =
    getEarliestBus timestamp busIds
        |> (\( t, id ) -> (t - timestamp) * id)


getEarliestBus : Int -> List Bus -> ( Int, Int )
getEarliestBus timestamp busIds =
    busIds
        |> List.filterMap (Maybe.filter (\id -> modBy id timestamp == 0))
        |> List.head
        |> Maybe.unpack
            (\_ -> getEarliestBus (timestamp + 1) busIds)
            (Tuple.pair timestamp)


part2 : ( a, List Bus ) -> Int
part2 ( _, busIds ) =
    earliestTimestamp 0 busIds


earliestTimestamp : Int -> List Bus -> Int
earliestTimestamp timestamp busIds =
    busIds
        |> List.indexedFoldl
            (\i busId result ->
                Maybe.filter (\a -> modBy a (timestamp + i) == 0) busId
                    |> Maybe.unwrap result (\a -> a :: result)
            )
            []
        |> (\matches ->
                if List.length matches == (List.filterMap identity busIds |> List.length) then
                    timestamp

                else
                    earliestTimestamp (timestamp + List.product matches) busIds
           )
