module Solvers.Y2020.Day13Test exposing (suite)

import Expect
import Solvers.Y2020.Day13 as D13
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
939
7,13,x,x,59,x,31,19
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 13"
        [ Test.test "D13P1" <|
            \_ ->
                exampleInput
                    |> D13.partToSolver D13.part1
                    |> Expect.equal (Ok "295")
        , Test.test "D13P2" <|
            \_ ->
                exampleInput
                    |> D13.partToSolver D13.part2
                    |> Expect.equal (Ok "1068781")
        ]
