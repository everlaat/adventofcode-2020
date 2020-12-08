module Solvers.Y2020.Day8Test exposing (suite)

import Expect
import Solvers.Y2020.Day8 as D8
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
nop +0
acc +1
jmp +4
acc +3
jmp -3
acc -99
acc +1
jmp -4
acc +6
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 8"
        [ Test.test "D8P1" <|
            \_ ->
                exampleInput
                    |> D8.partToSolver D8.part1
                    |> Expect.equal (Ok "5")
        , Test.test "D8P2" <|
            \_ ->
                exampleInput
                    |> D8.partToSolver D8.part2
                    |> Expect.equal (Ok "8")
        ]
