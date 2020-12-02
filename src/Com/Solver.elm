module Com.Solver exposing (Solver, make, solve, toId, toLabel)


type Solver
    = Solver
        { year : Int
        , day : Int
        , part : Int
        , func : String -> Result String String
        }


make : Int -> Int -> Int -> (String -> Result String String) -> Solver
make year day part func =
    Solver { year = year, day = day, part = part, func = func }


solve : Solver -> String -> Result String String
solve (Solver { func }) =
    func


toLabel : Solver -> String
toLabel (Solver { year, day, part }) =
    [ String.fromInt year
    , " d"
    , String.fromInt day
    , " p"
    , String.fromInt part
    ]
        |> String.join ""


toId : Solver -> String
toId (Solver { year, day, part }) =
    [ String.fromInt year
    , String.fromInt day
    , String.fromInt part
    ]
        |> String.join ""
