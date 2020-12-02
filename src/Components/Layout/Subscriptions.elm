module Components.Layout.Subscriptions exposing (subscriptions)

import Components.Layout.Model exposing (Model)
import Components.Layout.Msg exposing (MsgIn(..))


subscriptions : Model -> Sub MsgIn
subscriptions model =
    Sub.none
