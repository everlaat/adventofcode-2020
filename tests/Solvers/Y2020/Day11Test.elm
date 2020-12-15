module Solvers.Y2020.Day11Test exposing (suite)

import Expect
import Solvers.Y2020.Day11 as D11 exposing (Seat(..))
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
L.LL.LL.LL
LLLLLLL.LL
L.L.L..L..
LLLL.LL.LL
L.LL.LL.LL
L.LLLLL.LL
..L.L.....
LLLLLLLLLL
L.LLLLLL.L
L.LLLLL.LL
"""


exampleInputRound1 : String
exampleInputRound1 =
    """
#.##.##.##
#######.##
#.#.#..#..
####.##.##
#.##.##.##
#.#####.##
..#.#.....
##########
#.######.#
#.#####.##
"""
        |> String.trim
        |> String.lines
        |> String.join ""


exampleInputRound3b : String
exampleInputRound3b =
    """
#.L#.##.L#
#L#####.LL
L.#.#..#..
##L#.##.##
#.##.#L.##
#.#####.#L
..#.#.....
LLL####LL#
#.L#####.L
#.L####.L#
"""
        |> String.trim
        |> String.lines
        |> String.join ""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 11"
        [ Test.test "getNeigbouringPositions 1,1" <|
            \_ ->
                {-
                   #0,0  #1,0  #2,0  3,0
                   #0,1  >1,1  #2,1  3,1
                   #0,2  #1,2  #2,2  3,2
                    0,3   1,3   2,3  3,3
                -}
                D11.getNeigbouringPositions 1 1
                    |> Expect.equal [ ( 0, 0 ), ( 1, 0 ), ( 2, 0 ), ( 0, 1 ), ( 2, 1 ), ( 0, 2 ), ( 2, 2 ), ( 1, 2 ) ]
        , Test.test "getNeigbouringPositions 0,1" <|
            \_ ->
                {-
                   #0,0  #1,0   2,0  3,0
                   >0,1  #1,1   2,1  3,1
                   #0,2  #1,2   2,2  3,2
                    0,3   1,3   2,3  3,3
                -}
                D11.getNeigbouringPositions 0 1
                    |> Expect.equal [ ( 0, 0 ), ( 1, 0 ), ( 1, 1 ), ( 1, 2 ), ( 0, 2 ) ]
        , Test.test "D11P1 - 1 round" <|
            \_ ->
                exampleInput
                    |> D11.inputToMatrix
                    |> D11.gameOfSeats
                    |> D11.matrixToString
                    |> Expect.equal exampleInputRound1
        , Test.test "D11P1" <|
            \_ ->
                exampleInput
                    |> D11.partToSolver D11.part1
                    |> Expect.equal (Ok "stabilised with `37` occupied seats after `5` generations")
        , Test.test "D11P2 - 1 round" <|
            \_ ->
                exampleInput
                    |> D11.inputToMatrix
                    |> D11.gameOfSeats2
                    |> D11.matrixToString
                    |> Expect.equal exampleInputRound1
        , Test.test "D11P2 - 3 rounds" <|
            \_ ->
                exampleInput
                    |> D11.inputToMatrix
                    |> D11.gameOfSeats2
                    |> D11.gameOfSeats2
                    |> D11.gameOfSeats2
                    |> D11.matrixToString
                    |> Expect.equal exampleInputRound3b
        , Test.test "D11P2" <|
            \_ ->
                exampleInput
                    |> D11.partToSolver D11.part2
                    |> Expect.equal (Ok "stabilised with `26` occupied seats after `6` generations")
        ]
