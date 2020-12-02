module Solvers.Y2020.Day2 exposing (part1, part2, solvers)

import Com.Solver as Solver exposing (Solver)


solvers : List Solver
solvers =
    [ Solver.make 2020 2 1 (part1 |> partToSolver)
    , Solver.make 2020 2 2 (part2 |> partToSolver)
    ]


partToSolver : (String -> String) -> (String -> Result String String)
partToSolver f =
    f >> Ok


part1 : String -> String
part1 =
    identity


part2 : String -> String
part2 =
    identity
