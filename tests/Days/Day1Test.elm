module Days.Day1Test exposing (suite)

import Days.Day1 as D01
import Expect
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
    1721
    979
    366
    299
    675
    1456
"""


simplifiedInput : String
simplifiedInput =
    """
    2
    2
    3
    3
    4
    4
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 1"
        [ Test.test "D1P1 simplified input" <|
            \_ ->
                D01.part1 6 simplifiedInput
                    -- 2 + 4 will be found before 3 + 3
                    |> Expect.equal (Just 8)
        , Test.test "D1P1 example input" <|
            \_ ->
                D01.part1 2020 exampleInput
                    |> Expect.equal (Just 514579)
        , Test.test "D1P2 example input" <|
            \_ ->
                D01.part2 2020 exampleInput
                    |> Expect.equal (Just 241861950)
        ]
