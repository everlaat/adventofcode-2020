module Solvers.Y2020.Day8 exposing (part1, part2, partToSolver, solvers)

import Array exposing (Array)
import Com.Solver as Solver exposing (Solver)
import List
import Maybe.Extra as Maybe


solvers : List Solver
solvers =
    [ Solver.make 2020 8 1 (part1 |> partToSolver)
    , Solver.make 2020 8 2 (part2 |> partToSolver)
    ]


partToSolver : (Instructions -> Maybe Int) -> (String -> Result String String)
partToSolver f =
    inputToInstructions
        >> f
        >> Maybe.map String.fromInt
        >> Result.fromMaybe "answer not found"


type alias Instructions =
    Array Operator


type Operator
    = NoOp Int
    | Accumulate Int
    | Jump Int


operatorFromString : String -> Maybe Operator
operatorFromString str =
    case
        String.replace "+" "" str
            |> String.trim
            |> String.split " "
    of
        [ opp, val ] ->
            String.toInt val
                |> Maybe.andThen
                    (case opp of
                        "nop" ->
                            Just << NoOp

                        "acc" ->
                            Just << Accumulate

                        "jmp" ->
                            Just << Jump

                        _ ->
                            always Nothing
                    )

        _ ->
            Nothing


inputToInstructions : String -> Instructions
inputToInstructions =
    String.trim
        >> String.lines
        >> List.foldl
            (\line array ->
                operatorFromString line
                    |> Maybe.unwrap array (\a -> Array.push a array)
            )
            Array.empty


part1 : Instructions -> Maybe Int
part1 instructions =
    case runInstructions instructions of
        Err a ->
            Just a

        Ok a ->
            Just a


runInstructions : Instructions -> Result Int Int
runInstructions =
    next 0 0 []


next : Int -> Int -> List Int -> Instructions -> Result Int Int
next pointer accumulated visited instructions =
    case Array.get pointer instructions of
        Nothing ->
            Ok accumulated

        Just operator ->
            let
                ( newPointer, newAccumulated ) =
                    case operator of
                        NoOp _ ->
                            ( pointer + 1, accumulated )

                        Accumulate value ->
                            ( pointer + 1, accumulated + value )

                        Jump value ->
                            ( pointer + value, accumulated )
            in
            if List.member newPointer visited then
                Err accumulated

            else
                next newPointer newAccumulated (pointer :: visited) instructions


part2 : Instructions -> Maybe Int
part2 instructions =
    List.range 0 (Array.length instructions)
        |> List.map
            (\index ->
                (case Array.get index instructions of
                    Just (Accumulate x) ->
                        Accumulate x

                    Just (Jump x) ->
                        NoOp x

                    Just (NoOp x) ->
                        Jump x

                    _ ->
                        NoOp 0
                )
                    |> (\a -> Array.set index a instructions)
            )
        |> List.filterMap (runInstructions >> Result.toMaybe)
        |> List.head
