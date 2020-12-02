module Bootstrap exposing (apply, factory)

import Actors as Actors exposing (Actors)
import Actors.Layout as Layout
import Actors.Solver as Solver
import Framework.Actor exposing (Pid, Process)
import Html exposing (Html)
import Model as Model exposing (Model)
import Msg exposing (Msg)


factory : Actors -> ( Pid, () ) -> ( Model, Msg )
factory actorName =
    case actorName of
        Actors.Layout ->
            Layout.actor.init

        Actors.Solver ->
            Solver.actor.init


apply : Model -> Process Model (Html Msg) Msg
apply model =
    case model of
        Model.Layout m ->
            Layout.actor.apply m

        Model.Solver m ->
            Solver.actor.apply m
