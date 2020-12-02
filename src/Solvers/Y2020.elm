module Solvers.Y2020 exposing (solvers)

import Com.Solver as Solver exposing (Solver)
import Dict exposing (Dict)
import Solvers.Y2020.Day1 as Y2020D1
import Solvers.Y2020.Day2 as Y2020D2


solvers : Dict String Solver
solvers =
    [ Y2020D1.solvers
    , Y2020D2.solvers
    ]
        |> List.concat
        |> List.map (\solver -> ( Solver.toId solver, solver ))
        |> Dict.fromList
