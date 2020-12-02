module Solvers.Y2020.Day2Test exposing (suite)

import Expect
import Parser
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
        [ Test.test "Parse password" <|
            \_ ->
                Parser.run D2.parserPassword "1-3 a: abcde"
                    |> Expect.equal (Ok { x = 1, y = 3, char = 'a', password = "abcde" })
        , Test.test "Parse passwords" <|
            \_ ->
                Parser.run D2.parserPasswords exampleInput
                    |> Expect.equal
                        (Ok
                            [ { x = 1, y = 3, char = 'a', password = "abcde" }
                            , { x = 1, y = 3, char = 'b', password = "cdefg" }
                            , { x = 2, y = 9, char = 'c', password = "ccccccccc" }
                            ]
                        )
        , Test.test "D2P1 exampleInput" <|
            \_ ->
                exampleInput
                    |> D2.partToSolver D2.part1
                    |> Expect.equal (Ok "2")
        , Test.test "D2P2 exampleInput" <|
            \_ ->
                exampleInput
                    |> D2.partToSolver D2.part2
                    |> Expect.equal (Ok "1")
        ]
