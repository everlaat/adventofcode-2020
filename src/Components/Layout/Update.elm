module Components.Layout.Update exposing (update)

import Components.Layout.Model exposing (Model)
import Components.Layout.Msg exposing (MsgIn(..), MsgOut(..))
import Dict
import Solvers.Y2020 as Y2020


update : MsgIn -> Model -> ( Model, List MsgOut, Cmd MsgIn )
update msg model =
    case msg of
        OnInput input ->
            ( { model
                | input = input
              }
            , []
            , Cmd.none
            )

        OnSelectSolver solverId ->
            ( { model
                | solver = Dict.get solverId Y2020.solvers
              }
            , []
            , Cmd.none
            )

        OnSolve ->
            case model.solver of
                Nothing ->
                    ( model, [], Cmd.none )

                Just solver ->
                    ( { model
                        | input = ""
                      }
                    , [ Solve solver model.input
                      ]
                    , Cmd.none
                    )

        OnSpawnedSolver pid ->
            ( { model
                | solutions = pid :: model.solutions
              }
            , []
            , Cmd.none
            )
