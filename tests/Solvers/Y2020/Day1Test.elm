module Solvers.Y2020.Day1Test exposing (suite)

import Expect
import Solvers.Y2020.Day1 as D1
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
        [ Test.test "D1P1 no input" <|
            \_ ->
                D1.part1 1 ""
                    |> Expect.equal Nothing
        , Test.test "D1P1 simplified input" <|
            \_ ->
                D1.part1 6 simplifiedInput
                    -- 2 + 4 will be found before 3 + 3
                    |> Expect.equal (Just (2 * 4))
        , Test.test "D1P1 example input" <|
            \_ ->
                D1.part1 2020 exampleInput
                    |> Expect.equal (Just 514579)
        , Test.test "D1P2 simplified input (1)" <|
            \_ ->
                D1.part2 10 simplifiedInput
                    -- 2 + 4 + 4 will be found before 2 + 4 + 4
                    |> Expect.equal (Just (2 * 4 * 4))
        , Test.test "D1P2 simplified input (11)" <|
            \_ ->
                D1.part2 11 simplifiedInput
                    -- 3 + 4 + 4
                    |> Expect.equal (Just (3 * 4 * 4))
        , Test.test "D1P2 example input" <|
            \_ ->
                D1.part2 2020 exampleInput
                    |> Expect.equal (Just 241861950)
        ]
