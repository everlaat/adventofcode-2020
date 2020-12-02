module Components.Solver.Msg exposing (MsgIn(..), MsgOut(..))

import Com.Solver exposing (Solver)


type MsgIn
    = Solve Solver String


type MsgOut
    = NoMsgOut
