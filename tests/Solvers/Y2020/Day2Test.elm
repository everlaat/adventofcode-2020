module Solvers.Y2020.Day2Test exposing (suite)

import Expect
import Solvers.Y2020.Day2 as D2
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
1-3 a: abcde
1-3 b: cdefg
2-9 c: ccccccccc
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 2"
        [ Test.test "D2P1 exampleInput" <|
            \_ ->
                D2.part1 exampleInput |> Expect.equal "2"
        , Test.test "D2P2 exampleInput" <|
            \_ ->
                D2.part2 exampleInput |> Expect.equal "1"
        ]
