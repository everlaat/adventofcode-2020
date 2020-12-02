module Actors.Solver exposing (actor)

import Actors as Actors
import Address as Address
import Components.Solver.Main as Solver
import Components.Solver.Model as Solver
import Components.Solver.Msg as Solver
import Framework.Actor as Actor exposing (Actor, Pid)
import Framework.Message as Message
import Html exposing (Html)
import Model as Model
import Msg as Msg


type alias Model =
    Solver.Model


type alias MsgIn =
    Solver.MsgIn


type alias MsgOut =
    Solver.MsgOut


actor : Actor () Model Model.Model (Html Msg.Msg) Msg.Msg
actor =
    Solver.component
        |> Actor.fromComponent
            { toAppModel = Model.Solver
            , toAppMsg = Msg.Solver
            , fromAppMsg = fromAppMsg
            , onMsgOut = onMsgOut
            }


fromAppMsg : Msg.AppMsg -> Maybe MsgIn
fromAppMsg appMsg =
    case appMsg of
        Msg.Solver msgIn ->
            Just msgIn

        _ ->
            Nothing


onMsgOut : { self : Pid, msgOut : MsgOut } -> Msg.Msg
onMsgOut { self, msgOut } =
    Message.noOperation
