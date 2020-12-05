module Solvers.Y2020.Day5Test exposing (suite)

import Expect
import Solvers.Y2020.Day5 as D5
import Test exposing (Test)


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 5"
        [ Test.test "D5P1 BFFFBBFRRR" <|
            \_ ->
                "BFFFBBFRRR"
                    |> D5.partToSolver D5.part1
                    |> Expect.equal (Ok "567")
        , Test.test "D5P1 FFFBBBFRRR" <|
            \_ ->
                "FFFBBBFRRR"
                    |> D5.partToSolver D5.part1
                    |> Expect.equal (Ok "119")
        , Test.test "D5P1 BBFFBBFRLL" <|
            \_ ->
                "BBFFBBFRLL"
                    |> D5.partToSolver D5.part1
                    |> Expect.equal (Ok "820")
        ]
