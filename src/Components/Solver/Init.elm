module Components.Solver.Init exposing (init)

import Components.Solver.Model exposing (Model)
import Components.Solver.Msg exposing (MsgIn(..), MsgOut(..))


init : a -> ( Model, List MsgOut, Cmd MsgIn )
init _ =
    ( { solver = Nothing
      , input = ""
      , output = Nothing
      }
    , []
    , Cmd.none
    )
