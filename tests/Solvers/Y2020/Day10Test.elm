module Solvers.Y2020.Day10Test exposing (suite)

import Expect
import Solvers.Y2020.Day10 as D10
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
16
10
15
5
1
11
7
19
6
12
4
"""


exampleInput2 : String
exampleInput2 =
    """
28
33
18
42
31
14
46
20
48
47
24
23
49
45
19
38
39
11
1
32
25
35
8
17
7
9
4
2
34
10
3
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 10"
        [ Test.test "D10P1" <|
            \_ ->
                exampleInput
                    |> D10.partToSolver D10.part1
                    |> Expect.equal (Ok "35")
        , Test.test "D10P1 b" <|
            \_ ->
                exampleInput2
                    |> D10.partToSolver D10.part1
                    |> Expect.equal (Ok "220")
        , Test.test "D10P2" <|
            \_ ->
                exampleInput2
                    |> D10.partToSolver D10.part2
                    |> Expect.equal (Ok "19208")
        ]
