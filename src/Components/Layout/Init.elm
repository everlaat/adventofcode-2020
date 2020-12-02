module Components.Layout.Init exposing (init)

import Components.Layout.Model exposing (Model)
import Components.Layout.Msg exposing (MsgIn(..), MsgOut(..))


init : a -> ( Model, List MsgOut, Cmd MsgIn )
init _ =
    ( { input = ""
      , solver = Nothing
      , solutions = []
      }
    , []
    , Cmd.none
    )
