module Solvers.Y2020.Day6Test exposing (suite)

import Expect
import Solvers.Y2020.Day6 as D6
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
abc

a
b
c

ab
ac

a
a
a
a

b
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 6"
        [ Test.test "D6P1" <|
            \_ ->
                exampleInput
                    |> D6.partToSolver D6.part1
                    |> Expect.equal (Ok "11")
        , Test.test "D6P2" <|
            \_ ->
                exampleInput
                    |> D6.partToSolver D6.part2
                    |> Expect.equal (Ok "6")
        ]
