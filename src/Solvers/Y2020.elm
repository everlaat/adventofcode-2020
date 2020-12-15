module Solvers.Y2020 exposing (solvers)

import Com.Solver as Solver exposing (Solver)
import Dict exposing (Dict)
import Solvers.Y2020.Day1 as Y2020D1
import Solvers.Y2020.Day10 as Y2020D10
import Solvers.Y2020.Day11 as Y2020D11
import Solvers.Y2020.Day12 as Y2020D12
import Solvers.Y2020.Day13 as Y2020D13
import Solvers.Y2020.Day14 as Y2020D14
import Solvers.Y2020.Day15 as Y2020D15
import Solvers.Y2020.Day2 as Y2020D2
import Solvers.Y2020.Day3 as Y2020D3
import Solvers.Y2020.Day4 as Y2020D4
import Solvers.Y2020.Day5 as Y2020D5
import Solvers.Y2020.Day6 as Y2020D6
import Solvers.Y2020.Day7 as Y2020D7
import Solvers.Y2020.Day8 as Y2020D8
import Solvers.Y2020.Day9 as Y2020D9


solvers : Dict String Solver
solvers =
    [ Y2020D1.solvers
    , Y2020D2.solvers
    , Y2020D3.solvers
    , Y2020D4.solvers
    , Y2020D5.solvers
    , Y2020D6.solvers
    , Y2020D7.solvers
    , Y2020D8.solvers
    , Y2020D9.solvers
    , Y2020D10.solvers
    , Y2020D11.solvers
    , Y2020D12.solvers
    , Y2020D13.solvers
    , Y2020D14.solvers
    , Y2020D15.solvers
    ]
        |> List.concat
        |> List.map (\solver -> ( Solver.toId solver, solver ))
        |> Dict.fromList
