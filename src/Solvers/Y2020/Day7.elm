module Solvers.Y2020.Day7 exposing (part1, part2, partToSolver, solvers)

import Com.Solver as Solver exposing (Solver)
import Dict exposing (Dict)
import List
import List.Extra as List
import Maybe.Extra as Maybe


solvers : List Solver
solvers =
    [ Solver.make 2020 7 1 (part1 |> partToSolver)
    , Solver.make 2020 7 2 (part2 |> partToSolver)
    ]


partToSolver : (BagDict -> Maybe Int) -> (String -> Result String String)
partToSolver f =
    inputToBagDict
        >> f
        >> Maybe.map String.fromInt
        >> Result.fromMaybe "answer not found"


strRemove : List String -> String -> String
strRemove list str =
    List.foldl (\a -> String.replace a "") str list


type Bag
    = Bag String Bags


type alias Bags =
    List Bag


type alias BagDict =
    Dict String (List ( String, Int ))


inputToBagDict : String -> BagDict
inputToBagDict =
    String.trim
        >> strRemove [ "bags", "bag", "." ]
        >> String.lines
        >> List.filterMap
            (\line ->
                case String.split " contain " line of
                    [ bag, contents ] ->
                        String.split "," contents
                            |> List.filterMap
                                (\a ->
                                    case String.trim a |> String.split " " of
                                        amount :: rest ->
                                            String.toInt amount
                                                |> Maybe.map (\int -> ( String.join " " rest, int ))

                                        _ ->
                                            Nothing
                                )
                            |> (\bags -> Just ( String.trim bag, bags ))

                    _ ->
                        Nothing
            )
        >> Dict.fromList


bagDictToBags : BagDict -> Bags
bagDictToBags bagDict =
    Dict.toList bagDict
        |> List.map
            (\( name, list ) ->
                bagListToBags bagDict list
                    |> Bag name
            )


bagDictItemToBags : BagDict -> String -> Bags
bagDictItemToBags bagDict item =
    Dict.get item bagDict
        |> Maybe.unwrap [] (bagListToBags bagDict)


bagListToBags : BagDict -> List ( String, Int ) -> Bags
bagListToBags bagDict =
    List.map
        (\( k, v ) ->
            Bag k
                (bagDictItemToBags bagDict k)
                |> List.repeat v
        )
        >> List.concat


part1 : BagDict -> Maybe Int
part1 dict =
    let
        query =
            "shiny gold"
    in
    Dict.toList dict
        |> List.filter (Tuple.second >> search dict query)
        |> List.length
        |> Just


search : Dict String (List ( String, Int )) -> String -> List ( String, Int ) -> Bool
search dict query children =
    if
        children
            |> List.map Tuple.first
            |> List.member query
    then
        True

    else
        children
            |> List.map Tuple.first
            |> List.filterMap (\a -> Dict.get a dict |> Maybe.map (search dict query))
            |> List.any identity


part2 : BagDict -> Maybe Int
part2 bagDict =
    let
        query =
            "shiny gold"

        bags =
            bagDictItemToBags bagDict query
    in
    countBagsInBags bags
        |> Just


countBagsInBags : Bags -> Int
countBagsInBags bags =
    bags
        |> List.map (\(Bag _ list) -> countBagsInBags list)
        |> List.foldl (+) 0
        |> (\a -> a + List.length bags)
