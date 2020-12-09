module Solvers.Y2020.Day9Test exposing (suite)

import Expect
import Solvers.Y2020.Day9 as D9
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
35
20
15
25
47
40
62
55
65
95
102
117
150
182
127
219
299
277
309
576
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 9"
        [ Test.test "D9P1" <|
            \_ ->
                exampleInput
                    |> D9.partToSolver (D9.part1 5)
                    |> Expect.equal (Ok "127")
        , Test.test "D9P2" <|
            \_ ->
                exampleInput
                    |> D9.partToSolver (D9.part2 5)
                    |> Expect.equal (Ok "62")
        ]
