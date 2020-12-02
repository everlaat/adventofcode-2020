module Components.Layout.Msg exposing (MsgIn(..), MsgOut(..))

import Com.Solver exposing (Solver)
import Framework.Actor exposing (Pid)


type MsgIn
    = OnInput String
    | OnSelectSolver String
    | OnSolve
    | OnSpawnedSolver Pid


type MsgOut
    = Solve Solver String
