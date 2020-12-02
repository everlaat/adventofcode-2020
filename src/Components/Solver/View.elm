module Components.Solver.View exposing (view)

import Com.Solver as Solver exposing (Solver)
import Components.Solver.Model exposing (Model)
import Components.Solver.Msg exposing (MsgIn(..))
import Framework.Actor exposing (Pid)
import Html exposing (Html)
import Html.Attributes as HtmlA


view : a -> Model -> b -> Html msg
view _ model _ =
    case ( model.output, model.solver ) of
        ( Just result, Just solver ) ->
            viewResult solver model.input result

        _ ->
            Html.text ""


viewResult : Solver -> String -> Result String String -> Html msg
viewResult solver input result =
    Html.div [ HtmlA.class "Solver" ]
        [ Html.div [ HtmlA.class "Solver_Title" ]
            [ Html.text "--"
            , Solver.toLabel solver |> Html.text
            , Html.text "--"
            ]
        , Html.div [ HtmlA.class "Solver_Input" ]
            [ Html.textarea
                [ HtmlA.disabled True
                ]
                [ Html.text input ]
            ]
        , Html.div [ HtmlA.class "Solver_Result" ]
            [ Html.code []
                [ Html.pre []
                    [ case result of
                        Ok output ->
                            Html.text output

                        Err output ->
                            Html.text output
                    ]
                ]
            ]
        ]
