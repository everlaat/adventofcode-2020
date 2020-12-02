module Components.Layout.Model exposing (Model)

import Com.Solver exposing (Solver)
import Framework.Actor exposing (Pid)


type alias Model =
    { input : String
    , solver : Maybe Solver
    , solutions : List Pid
    }
