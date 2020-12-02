module Solvers.Y2020.Day2Test exposing (suite)

import Expect
import Solvers.Y2020.Day2 as D2
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
1
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 2"
        [ Test.test "D2P1 exampleInput" <|
            \_ ->
                D2.part1 exampleInput |> Expect.equal exampleInput
        , Test.test "D2P2 exampleInput" <|
            \_ ->
                D2.part1 exampleInput |> Expect.equal exampleInput
        ]
