module Solvers.Y2020.Day3Test exposing (suite)

import Expect
import Solvers.Y2020.Day3 as D3
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
..##.......
#...#...#..
.#....#..#.
..#.#...#.#
.#...##..#.
..#.##.....
.#.#.#....#
.#........#
#.##...#...
#...##....#
.#..#...#.#
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 3"
        [ Test.test "D3P1 exampleInput" <|
            \_ ->
                exampleInput
                    |> D3.partToSolver D3.part1
                    |> Expect.equal (Ok "7")
        , Test.test "D3P2 exampleInput" <|
            \_ ->
                exampleInput
                    |> D3.partToSolver D3.part2
                    |> Expect.equal (Ok "336")
        ]
