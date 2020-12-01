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


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 1"
        [ Test.test "D1P1 example input" <|
            \_ ->
                D01.part1 exampleInput
                    |> Expect.equal (Just 514579)

        -- , Test.test "D1P1 puzzle input" <|
        --     \_ ->
        --         D01.part1 puzzleInput
        --             |> Expect.equal (Just _)
        , Test.test "D1P2 example input" <|
            \_ ->
                D01.part2 exampleInput
                    |> Expect.equal (Just 241861950)

        -- , Test.test "D1P2 puzzle input" <|
        --     \_ ->
        --         D01.part2 puzzleInput
        --             |> Expect.equal (Just _)
        ]
