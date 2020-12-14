module Solvers.Y2020.Day11 exposing (Seat(..), gameOfSeats, inputToMatrix, matrixToString, part1, part2, partToSolver, solvers)

import Array
import Com.Solver as Solver exposing (Solver)
import List
import List.Extra as List
import Matrix exposing (Matrix)
import Maybe
import Maybe.Extra as Maybe
import Neighbours


solvers : List Solver
solvers =
    [ Solver.make 2020 11 1 (part1 |> partToSolver)
    , Solver.make 2020 11 2 (part2 |> partToSolver)
    ]


type Seat
    = Empty
    | Occupied
    | NoSeat


partToSolver : (Matrix Seat -> Maybe Int) -> (String -> Result String String)
partToSolver f =
    inputToMatrix
        >> f
        >> Maybe.map String.fromInt
        >> Result.fromMaybe "answer not found"


inputToMatrix : String -> Matrix Seat
inputToMatrix =
    String.trim
        >> String.lines
        >> List.map
            (\row ->
                let
                    charsOnRow =
                        String.toList row
                            -- |> List.map charToSeat
                            |> Array.fromList
                in
                Matrix.generate
                    (Array.length charsOnRow)
                    1
                    (\x _ -> Array.get x charsOnRow |> Maybe.unwrap NoSeat charToSeat)
            )
        >> List.map Ok
        >> List.foldl1
            (\a b ->
                Result.map2 Matrix.concatVertical b a
                    |> Result.andThen identity
            )
        >> Maybe.withDefault (Err "list was empty")
        >> Result.withDefault (Matrix.generate 0 0 (\_ _ -> Empty))


charToSeat : Char -> Seat
charToSeat char =
    case char of
        'L' ->
            Empty

        '#' ->
            Occupied

        _ ->
            NoSeat


part1 : Matrix Seat -> Maybe Int
part1 =
    run
        >> Matrix.toArray
        >> Array.toList
        >> List.filter ((==) Occupied)
        >> List.length
        >> Just


part2 : Matrix Seat -> Maybe Int
part2 matrix =
    Nothing


run : Matrix Seat -> Matrix Seat
run matrix =
    let
        newMatrix =
            gameOfSeats matrix
    in
    if matrixEquals matrix newMatrix then
        newMatrix

    else
        run newMatrix


gameOfSeats : Matrix Seat -> Matrix Seat
gameOfSeats matrix =
    let
        width =
            Matrix.width matrix

        height =
            Matrix.height matrix
    in
    (width * height)
        |> List.range 0
        |> List.foldl
            (\index m_ ->
                let
                    x =
                        remainderBy height index

                    y =
                        index // width

                    occupied =
                        Neighbours.neighbours Neighbours.Plane x y matrix
                            -- |> Debug.log "neighbours"
                            |> Array.toList
                            |> List.filter ((==) Occupied)
                            |> List.length
                in
                case Matrix.get x y matrix of
                    Ok Empty ->
                        if occupied == 0 then
                            Matrix.set x y Occupied m_

                        else
                            m_

                    Ok Occupied ->
                        if occupied >= 4 then
                            Matrix.set x y Empty m_

                        else
                            m_

                    _ ->
                        m_
            )
            matrix


matrixEquals : Matrix Seat -> Matrix Seat -> Bool
matrixEquals a b =
    a == b


matrixToString : Matrix Seat -> String
matrixToString matrix =
    Matrix.toArray matrix
        |> Array.toList
        |> List.map seatToChar
        |> String.fromList


seatToChar : Seat -> Char
seatToChar seat =
    case seat of
        Empty ->
            'L'

        Occupied ->
            '#'

        NoSeat ->
            '.'
