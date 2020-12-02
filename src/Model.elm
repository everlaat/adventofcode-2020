module Model exposing (Model(..))

import Components.Layout.Model as Layout
import Components.Solver.Model as Solver


type Model
    = Layout Layout.Model
    | Solver Solver.Model
