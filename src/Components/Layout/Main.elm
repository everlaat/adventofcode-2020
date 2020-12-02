module Components.Layout.Main exposing (component)

import Components.Layout.Init exposing (init)
import Components.Layout.Model exposing (Model)
import Components.Layout.Msg exposing (MsgIn, MsgOut)
import Components.Layout.Subscriptions exposing (subscriptions)
import Components.Layout.Update exposing (update)
import Components.Layout.View exposing (view)
import Framework.Actor exposing (Component)
import Html exposing (Html)


component : Component () Model MsgIn MsgOut (Html msg) msg
component =
    { init = init
    , update = update
    , subscriptions = subscriptions
    , view = view
    }
