module Solvers.Y2020.Day11 exposing (Seat(..), gameOfSeats, gameOfSeats2, getNeigbouringPositions, inputToMatrix, matrixToString, part1, part2, partToSolver, solvers)

import Array exposing (Array)
import Com.Solver as Solver exposing (Solver)
import List
import List.Extra as List
import Maybe
import Maybe.Extra as Maybe


solvers : List Solver
solvers =
    [ Solver.make 2020 11 1 (part1 |> partToSolver)
    , Solver.make 2020 11 2 (part2 |> partToSolver)
    ]


type Seat
    = Empty
    | Occupied
    | NoSeat


type alias Matrix =
    Array (Array Seat)


maxIterations : Int
maxIterations =
    9999


partToSolver : (Matrix -> Result Int ( Int, Int )) -> (String -> Result String String)
partToSolver f =
    inputToMatrix
        >> f
        >> Result.map
            (\( a, b ) ->
                "stabilised with `%b` occupied seats after `%a` generations"
                    |> String.replace "%a" (String.fromInt a)
                    |> String.replace "%b" (String.fromInt b)
            )
        >> Result.mapError String.fromInt


inputToMatrix : String -> Matrix
inputToMatrix =
    String.trim
        >> String.lines
        >> List.map (String.toList >> List.map charToSeat >> Array.fromList)
        >> Array.fromList


charToSeat : Char -> Seat
charToSeat char =
    case char of
        'L' ->
            Empty

        '#' ->
            Occupied

        _ ->
            NoSeat


part1 : Matrix -> Result Int ( Int, Int )
part1 =
    runGameOfSeatsUntilItResolves gameOfSeats
        >> Result.map
            (Tuple.mapSecond
                (array2dToList
                    >> List.filter ((==) Occupied)
                    >> List.length
                )
            )


array2dToList : Array (Array a) -> List a
array2dToList =
    Array.toList
        >> List.map Array.toList
        >> List.concat


runGameOfSeatsUntilItResolves : (Matrix -> Matrix) -> Matrix -> Result Int ( Int, Matrix )
runGameOfSeatsUntilItResolves f =
    run f 0


run : (Matrix -> Matrix) -> Int -> Matrix -> Result Int ( Int, Matrix )
run f iteration matrix =
    let
        newMatrix =
            f matrix
    in
    if iteration >= maxIterations then
        Err iteration

    else if matrixEquals matrix newMatrix then
        Ok ( iteration, newMatrix )

    else
        run f (iteration + 1) newMatrix


gameOfSeats : Matrix -> Matrix
gameOfSeats matrix =
    Array.indexedMap
        (\y ->
            Array.indexedMap
                (\x seat ->
                    case seat of
                        Empty ->
                            if amountOfOccupiedIsZero x y matrix then
                                Occupied

                            else
                                Empty

                        Occupied ->
                            if amountOfOccupiedIsFourOrMore x y matrix then
                                Empty

                            else
                                Occupied

                        NoSeat ->
                            NoSeat
                )
        )
        matrix


amountOfOccupiedIsZero : Int -> Int -> Matrix -> Bool
amountOfOccupiedIsZero x y matrix =
    getNeigbouringPositions x y
        |> List.foldl
            (\( a, b ) result ->
                if result == False then
                    False

                else
                    getSeatAt a b matrix /= Just Occupied
            )
            True


amountOfOccupiedIsFourOrMore : Int -> Int -> Matrix -> Bool
amountOfOccupiedIsFourOrMore x y matrix =
    getNeigbouringPositions x y
        |> List.foldl
            (\( a, b ) result ->
                if result >= 4 then
                    result

                else if getSeatAt a b matrix == Just Occupied then
                    result + 1

                else
                    result
            )
            0
        |> (\a -> a >= 4)


getNeigbouringPositions : Int -> Int -> List ( Int, Int )
getNeigbouringPositions x y =
    [ ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    , ( -1, 0 )
    , ( 1, 0 )
    , ( -1, 1 )
    , ( 1, 1 )
    , ( 0, 1 )
    ]
        |> List.filterMap
            (\( a, b ) ->
                if a + x >= 0 && b + y >= 0 then
                    Just ( a + x, b + y )

                else
                    Nothing
            )


getSeatAt : Int -> Int -> Matrix -> Maybe Seat
getSeatAt x y matrix =
    Array.get y matrix
        |> Maybe.andThen (Array.get x)


matrixEquals : Matrix -> Matrix -> Bool
matrixEquals a b =
    a == b


matrixToString : Matrix -> String
matrixToString =
    array2dToList
        >> List.map seatToChar
        >> String.fromList


seatToChar : Seat -> Char
seatToChar seat =
    case seat of
        Empty ->
            'L'

        Occupied ->
            '#'

        NoSeat ->
            '.'


part2 : Matrix -> Result Int ( Int, Int )
part2 =
    runGameOfSeatsUntilItResolves gameOfSeats2
        >> Result.map
            (Tuple.mapSecond
                (array2dToList
                    >> List.filter ((==) Occupied)
                    >> List.length
                )
            )


gameOfSeats2 : Matrix -> Matrix
gameOfSeats2 matrix =
    Array.indexedMap
        (\y ->
            Array.indexedMap
                (\x seat ->
                    case seat of
                        Empty ->
                            if amountOfOccupiedIsZeroInLineOfSight x y matrix then
                                Occupied

                            else
                                Empty

                        Occupied ->
                            if amountOfOccupiedIsFiveOrMoreInLineOfSight x y matrix then
                                Empty

                            else
                                Occupied

                        NoSeat ->
                            NoSeat
                )
        )
        matrix


isOccupiedAlongLineOfSight : Int -> ( Int, Int ) -> Matrix -> ( Int, Int ) -> Bool
isOccupiedAlongLineOfSight distance ( a, b ) matrix ( x, y ) =
    let
        aa =
            (distance * a) + x

        bb =
            (distance * b) + y
    in
    getSeatAt aa bb matrix
        |> Maybe.unwrap False
            (\seat ->
                case seat of
                    Occupied ->
                        True

                    Empty ->
                        False

                    NoSeat ->
                        isOccupiedAlongLineOfSight (distance + 1) ( a, b ) matrix ( x, y )
            )


neighbourDirections : List ( Int, Int )
neighbourDirections =
    [ ( -1, -1 )
    , ( 0, -1 )
    , ( 1, -1 )
    , ( -1, 0 )
    , ( 1, 0 )
    , ( -1, 1 )
    , ( 1, 1 )
    , ( 0, 1 )
    ]


amountOfOccupiedIsZeroInLineOfSight : Int -> Int -> Matrix -> Bool
amountOfOccupiedIsZeroInLineOfSight x y matrix =
    List.foldl
        (\moveset result ->
            if result == False then
                False

            else
                not <| isOccupiedAlongLineOfSight 1 moveset matrix ( x, y )
        )
        True
        neighbourDirections


amountOfOccupiedIsFiveOrMoreInLineOfSight : Int -> Int -> Matrix -> Bool
amountOfOccupiedIsFiveOrMoreInLineOfSight x y matrix =
    neighbourDirections
        |> List.filter (\moveset -> isOccupiedAlongLineOfSight 1 moveset matrix ( x, y ))
        |> (\a -> List.length a >= 5)
