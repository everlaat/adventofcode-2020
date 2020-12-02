module Actors.Layout exposing (actor)

import Actors as Actors
import Components.Layout.Main as Layout
import Components.Layout.Model as Layout
import Components.Layout.Msg as Layout
import Components.Solver.Msg as Solver
import Framework.Actor as Actor exposing (Actor, Pid)
import Framework.Message as Message
import Html exposing (Html)
import Model as Model
import Msg as Msg


type alias Model =
    Layout.Model


type alias MsgIn =
    Layout.MsgIn


type alias MsgOut =
    Layout.MsgOut


actor : Actor () Model Model.Model (Html Msg.Msg) Msg.Msg
actor =
    Layout.component
        |> Actor.fromComponent
            { toAppModel = Model.Layout
            , toAppMsg = Msg.Layout
            , fromAppMsg = fromAppMsg
            , onMsgOut = onMsgOut
            }


fromAppMsg : Msg.AppMsg -> Maybe MsgIn
fromAppMsg appMsg =
    case appMsg of
        Msg.Layout msgIn ->
            Just msgIn

        _ ->
            Nothing


onMsgOut : { self : Pid, msgOut : MsgOut } -> Msg.Msg
onMsgOut { self, msgOut } =
    case msgOut of
        Layout.Solve solver input ->
            Message.spawn ()
                Actors.Solver
                (\pid ->
                    Message.batch
                        [ Solver.Solve solver input
                            |> Msg.Solver
                            |> Message.sendToPid pid
                        , Layout.OnSpawnedSolver pid
                            |> Msg.Layout
                            |> Message.sendToPid self
                        ]
                )
