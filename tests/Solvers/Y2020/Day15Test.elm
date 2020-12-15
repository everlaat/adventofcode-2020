module Solvers.Y2020.Day15Test exposing (suite)

import Expect
import Solvers.Y2020.Day15 as D15
import Test exposing (Test)


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 15"
        [ Test.test "D15P1" <|
            \_ ->
                "0,3,6"
                    |> D15.partToSolver D15.part1
                    |> Expect.equal (Ok "436")
        , Test.test "D15P1 b" <|
            \_ ->
                "2,1,3"
                    |> D15.partToSolver D15.part1
                    |> Expect.equal (Ok "10")
        ]
