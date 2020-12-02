module Main exposing (main)

import Actors as Actors exposing (Actors)
import Address as Address exposing (Address)
import Bootstrap exposing (apply, factory)
import Framework.Browser as Browser exposing (Program)
import Framework.Message as Message
import Html exposing (Html)
import Html.Attributes as HtmlA
import Model exposing (Model)
import Msg exposing (AppMsg, Msg)
import Styles


main : Program () () Address Actors Model AppMsg
main =
    Browser.document
        { apply = apply
        , factory = factory
        , init = init
        , view = view
        }


init : () -> Msg
init _ =
    let
        spawn actor address =
            Message.spawn
                ()
                actor
                (\pid ->
                    Message.batch
                        [ Message.addToView pid
                        , Message.populateAddress address pid
                        ]
                )
    in
    [ spawn Actors.Layout Address.Layout
    ]
        |> Message.batch


view : List (Html Msg) -> List (Html Msg)
view output =
    [ Html.div [ HtmlA.class "app" ]
        [ Styles.styleNode
        , Html.div [ HtmlA.class "container" ] output
        ]
    ]
