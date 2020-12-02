module Components.Layout.View exposing (view)

import Com.Solver as Solver exposing (Solver)
import Components.Layout.Model exposing (Model)
import Components.Layout.Msg exposing (MsgIn(..))
import Dict
import Framework.Actor exposing (Pid)
import Html exposing (Html)
import Html.Attributes as HtmlA
import Html.Events as HtmlE
import Solvers.Y2020 as Y2020


view : (MsgIn -> msg) -> Model -> (Pid -> Maybe (Html msg)) -> Html msg
view toSelf model renderPid =
    Html.div [ HtmlA.class "Layout" ]
        [ viewForm toSelf model
        , viewSolutions model renderPid
        ]


viewForm : (MsgIn -> msg) -> Model -> Html msg
viewForm toSelf model =
    Html.div [ HtmlA.class "Layout_Form" ]
        [ Html.div [ HtmlA.class "InputWrapper" ]
            [ Html.label [ HtmlA.for "input" ] [ Html.text "input" ]
            , Html.textarea
                [ HtmlA.name "input"
                , HtmlA.value model.input
                , HtmlE.onInput (OnInput >> toSelf)
                ]
                []
            ]
        , Html.div [ HtmlA.class "InputWrapper" ]
            [ Html.label [ HtmlA.for "solver" ] [ Html.text "solver" ]
            , Html.div [ HtmlA.class "SelectWrapper", HtmlE.onInput (OnSelectSolver >> toSelf) ]
                [ Html.span [] [ Html.text "[" ]
                , Html.select [ HtmlA.name "solver" ] (viewSolverOptions model.solver)
                , Html.span [] [ Html.text "]" ]
                ]
            ]
        , Html.button
            [ HtmlA.disabled (model.solver == Nothing)
            , HtmlE.onClick (OnSolve |> toSelf)
            ]
            [ Html.text "[Solve]" ]
        ]


viewSolverOptions : Maybe Solver -> List (Html msg)
viewSolverOptions maybeSelectedSolver =
    let
        selectedSolverId =
            Maybe.map Solver.toId maybeSelectedSolver
    in
    Dict.toList Y2020.solvers
        |> List.map
            (\( solverId, solver ) ->
                Html.option
                    [ HtmlA.value solverId
                    , HtmlA.selected (selectedSolverId == Just solverId)
                    ]
                    [ Solver.toLabel solver
                        |> Html.text
                    ]
            )
        |> (\options ->
                if selectedSolverId == Nothing then
                    Html.option [ HtmlA.selected True ] [ Html.text "" ] :: options

                else
                    options
           )


viewSolutions : Model -> (Pid -> Maybe (Html msg)) -> Html msg
viewSolutions model renderPid =
    model.solutions
        |> List.filterMap renderPid
        |> Html.div [ HtmlA.class "Layout_Solutions" ]
