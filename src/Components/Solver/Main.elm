module Components.Solver.Main exposing (component)

import Components.Solver.Init exposing (init)
import Components.Solver.Model exposing (Model)
import Components.Solver.Msg exposing (MsgIn, MsgOut)
import Components.Solver.Subscriptions exposing (subscriptions)
import Components.Solver.Update exposing (update)
import Components.Solver.View exposing (view)
import Framework.Actor exposing (Component)
import Html exposing (Html)


component : Component () Model MsgIn MsgOut (Html msg) msg
component =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
