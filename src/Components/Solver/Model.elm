module Components.Solver.Model exposing (Model)

import Com.Solver exposing (Solver)


type alias Model =
    { solver : Maybe Solver
    , input : String
    , output : Maybe (Result String String)
    }
