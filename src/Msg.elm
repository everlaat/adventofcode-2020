module Msg exposing (AppMsg(..), Msg)

import Actors exposing (Actors)
import Address exposing (Address)
import Components.Layout.Msg as Layout
import Components.Solver.Msg as Solver
import Framework.Message exposing (FrameworkMessage)
import Model exposing (Model)


type alias Msg =
    FrameworkMessage () Address Actors Model AppMsg


type AppMsg
    = Layout Layout.MsgIn
    | Solver Solver.MsgIn
