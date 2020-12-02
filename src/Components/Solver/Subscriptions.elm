module Components.Solver.Subscriptions exposing (subscriptions)

import Components.Solver.Model exposing (Model)
import Components.Solver.Msg exposing (MsgIn(..))


subscriptions : Model -> Sub MsgIn
subscriptions model =
    Sub.none
