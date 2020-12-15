module Solvers.Y2020.Day12 exposing (part1, part2, partToSolver, solvers)

import Com.Solver as Solver exposing (Solver)
import List
import List.Extra as List


solvers : List Solver
solvers =
    [ Solver.make 2020 12 1 (part1 |> partToSolver)
    , Solver.make 2020 12 2 (part2 |> partToSolver)
    ]


partToSolver : (Instructions -> Ship) -> (String -> Result String String)
partToSolver f =
    inputToInstructions
        >> f
        -- >> Debug.toString >> Ok
        >> shipToManhattenDistance
        >> String.fromInt
        >> Ok


shipToManhattenDistance : Ship -> Int
shipToManhattenDistance { x, y } =
    abs x + abs y


inputToInstructions : String -> Instructions
inputToInstructions =
    String.trim
        >> String.lines
        >> List.filterMap
            (String.uncons
                >> Maybe.andThen
                    (\( a, b ) ->
                        String.toInt b
                            |> Maybe.map (Tuple.pair a)
                    )
                >> Maybe.andThen
                    (\( char, int ) ->
                        case char of
                            'N' ->
                                Just <| Course North int

                            'E' ->
                                Just <| Course East int

                            'S' ->
                                Just <| Course South int

                            'W' ->
                                Just <| Course West int

                            'L' ->
                                Just <| Rotate CCW int

                            'R' ->
                                Just <| Rotate CW int

                            'F' ->
                                Just <| Forward int

                            _ ->
                                Nothing
                    )
            )


type alias Instructions =
    List Operation


type Operation
    = Course CardinalDirection Distance
    | Rotate Rotation Degrees
    | Forward Distance


type alias Degrees =
    Int


type alias Distance =
    Int


type Rotation
    = CW
    | CCW


type CardinalDirection
    = North
    | East
    | South
    | West


type alias Ship =
    { heading : CardinalDirection
    , x : Int
    , y : Int
    }


part1 : Instructions -> Ship
part1 =
    List.foldl
        doOperation
        { heading = East
        , x = 0
        , y = 0
        }


doOperation : Operation -> Ship -> Ship
doOperation operation ship =
    case operation of
        Course North distance ->
            { ship | y = ship.y - distance }

        Course South distance ->
            { ship | y = ship.y + distance }

        Course East distance ->
            { ship | x = ship.x + distance }

        Course West distance ->
            { ship | x = ship.x - distance }

        Rotate CW degrees ->
            { ship | heading = degreesToCardinalDirection (cardinalDirectionToDegrees ship.heading + degrees) }

        Rotate CCW degrees ->
            { ship | heading = degreesToCardinalDirection (cardinalDirectionToDegrees ship.heading - degrees) }

        Forward distance ->
            doOperation (Course ship.heading distance) ship


cardinalDirectionToDegrees : CardinalDirection -> Int
cardinalDirectionToDegrees cardinalDirection =
    case cardinalDirection of
        North ->
            0

        East ->
            90

        South ->
            180

        West ->
            270


degreesToCardinalDirection : Int -> CardinalDirection
degreesToCardinalDirection degrees =
    let
        a =
            modBy 360 degrees
    in
    if a == 0 then
        North

    else if a == 90 then
        East

    else if a == 180 then
        South

    else
        West


part2 : Instructions -> Ship
part2 =
    List.foldl
        doOperationB
        ( { heading = East
          , x = 0
          , y = 0
          }
        , { x = 10, y = 1 }
        )
        >> Tuple.first


type alias Waypoint =
    { x : Int
    , y : Int
    }


doOperationB : Operation -> ( Ship, Waypoint ) -> ( Ship, Waypoint )
doOperationB operation ( ship, waypoint ) =
    case operation of
        Course North distance ->
            ( ship, { waypoint | y = waypoint.y + distance } )

        Course South distance ->
            ( ship, { waypoint | y = waypoint.y - distance } )

        Course East distance ->
            ( ship, { waypoint | x = waypoint.x + distance } )

        Course West distance ->
            ( ship, { waypoint | x = waypoint.x - distance } )

        Rotate rotation degrees ->
            ( ship, rotateWaypoint rotation degrees waypoint )

        Forward distance ->
            ( { ship
                | x = ship.x + distance * waypoint.x
                , y = ship.y + distance * waypoint.y
              }
            , waypoint
            )


rotateWaypoint : Rotation -> Degrees -> Waypoint -> Waypoint
rotateWaypoint rotation degrees waypoint =
    if degrees == 0 then
        waypoint

    else
        case rotation of
            CW ->
                rotateWaypoint rotation (degrees - 90) { x = waypoint.y, y = -waypoint.x }

            CCW ->
                rotateWaypoint rotation (degrees - 90) { x = -waypoint.y, y = waypoint.x }
