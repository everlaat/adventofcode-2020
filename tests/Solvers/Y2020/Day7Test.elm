module Solvers.Y2020.Day7Test exposing (suite)

import Expect
import Solvers.Y2020.Day7 as D7
import Test exposing (Test)


exampleInput : String
exampleInput =
    """
light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.
"""


example2Input : String
example2Input =
    """
shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags.
"""


suite : Test
suite =
    Test.describe "AdventOfCode 2020 Day 7"
        [ Test.test "D7P1" <|
            \_ ->
                exampleInput
                    |> D7.partToSolver D7.part1
                    |> Expect.equal (Ok "4")
        , Test.test "D7P2" <|
            \_ ->
                exampleInput
                    |> D7.partToSolver D7.part2
                    |> Expect.equal (Ok "32")
        , Test.test "D7P2 II" <|
            \_ ->
                example2Input
                    |> D7.partToSolver D7.part2
                    |> Expect.equal (Ok "126")
        ]
