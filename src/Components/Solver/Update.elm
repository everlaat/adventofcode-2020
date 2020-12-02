module Components.Solver.Update exposing (update)

import Com.Solver as Solver
import Components.Solver.Model exposing (Model)
import Components.Solver.Msg exposing (MsgIn(..), MsgOut(..))


update : MsgIn -> Model -> ( Model, List MsgOut, Cmd MsgIn )
update msg _ =
    case msg of
        Solve solver input ->
            ( { input = input
              , solver = Just solver
              , output = Just <| Solver.solve solver input
              }
            , []
            , Cmd.none
            )
