module Solvers.Y2020.Day14Test exposing (suite)

import Expect
import Solvers.Y2020.Day14 as D14
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
mask = XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X
mem[8] = 11
mem[7] = 101
mem[8] = 0
"""


exampleInput2 : String
exampleInput2 =
    """
mask = 000000000000000000000000000000X1001X
mem[42] = 100
mask = 00000000000000000000000000000000X0XX
mem[26] = 1
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 14"
        [ Test.test "D14P1" <|
            \_ ->
                exampleInput
                    |> D14.partToSolver D14.part1
                    |> Expect.equal (Ok "165")
        , Test.test "D14P2" <|
            \_ ->
                exampleInput2
                    |> D14.partToSolver D14.part2
                    |> Expect.equal (Ok "208")
        ]
