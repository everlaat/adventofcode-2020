module Solvers.Y2020.Day12Test exposing (suite)

import Expect
import Solvers.Y2020.Day12 as D12
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
F10
N3
F7
R90
F11
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 12"
        [ Test.test "D12P1" <|
            \_ ->
                exampleInput
                    |> D12.partToSolver D12.part1
                    |> Expect.equal (Ok "25")
        , Test.test "D12P2" <|
            \_ ->
                exampleInput
                    |> D12.partToSolver D12.part2
                    |> Expect.equal (Ok "286")
        ]
