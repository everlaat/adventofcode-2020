module Solvers.Y2020.Day7 exposing (part1, part2, partToSolver, solvers)

import Com.Solver as Solver exposing (Solver)
import Dict exposing (Dict)
import List
import List.Extra as List
import Maybe.Extra as Maybe
import Tuple.Extra as Tuple


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
    = Bag String (() -> FitsBags)


type alias FitsBags =
    List ( Int, Bag )


type alias BagDict =
    Dict String Bag


toFitsBags : Bag -> FitsBags
toFitsBags (Bag _ f) =
    f ()


toName : Bag -> String
toName (Bag name _) =
    name


inputToBagDict : String -> BagDict
inputToBagDict =
    let
        bagDictItemToBag : Dict String (List ( String, Int )) -> String -> Bag
        bagDictItemToBag bagDict bagName =
            Dict.get bagName bagDict
                |> Maybe.unwrap [] (List.map (Tuple.flip >> Tuple.mapSecond (bagDictItemToBag bagDict)))
                |> always
                |> Bag bagName
    in
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
        >> (\dict -> Dict.map (\k _ -> bagDictItemToBag dict k) dict)


part1 : BagDict -> Maybe Int
part1 =
    Dict.toList
        >> List.filter (Tuple.second >> flattenFitsBags >> List.member "shiny gold")
        >> List.length
        >> Just


flattenFitsBags : Bag -> List String
flattenFitsBags =
    toFitsBags
        >> List.foldl
            (\( _, a ) -> List.append (toName a :: flattenFitsBags a))
            -- could count how many would fit
            -- (\( a, b ) c ->
            -- List.repeat a (toName b)
            --     |> (\d -> List.concat [ d, c, flattenFitsBags b ])
            -- )
            []


part2 : BagDict -> Maybe Int
part2 =
    Dict.get "shiny gold"
        >> Maybe.map (toFitsBags >> countBagsInBags >> (\a -> a - 1))


countBagsInBags : FitsBags -> Int
countBagsInBags fitsBags =
    fitsBags
        |> List.map (Tuple.mapSecond (toFitsBags >> countBagsInBags) >> Tuple.apply (*))
        |> List.foldl (+) 1
