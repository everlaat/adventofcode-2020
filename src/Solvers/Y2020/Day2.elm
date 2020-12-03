module Solvers.Y2020.Day2 exposing (parserPassword, parserPasswords, part1, part2, partToSolver, solvers)

import Com.Solver as Solver exposing (Solver)
import Parser exposing ((|.), (|=), Parser)


type alias Password =
    { x : Int
    , y : Int
    , char : Char
    , password : String
    }


solvers : List Solver
solvers =
    [ Solver.make 2020 2 1 (part1 |> partToSolver)
    , Solver.make 2020 2 2 (part2 |> partToSolver)
    ]


partToSolver : (Password -> Bool) -> (String -> Result String String)
partToSolver f =
    Parser.run parserPasswords
        >> Result.map (List.filter f >> List.length >> String.fromInt)
        >> Result.mapError Parser.deadEndsToString


part1 : Password -> Bool
part1 { x, y, char, password } =
    let
        amount =
            String.indices (String.fromChar char) password
                |> List.length
    in
    amount >= x && amount <= y


part2 : Password -> Bool
part2 { x, y, char, password } =
    let
        indices =
            String.indices (String.fromChar char) password
                |> List.map ((+) 1)
    in
    xor
        (List.member x indices)
        (List.member y indices)



-- Parsing


parserPasswords : Parser (List Password)
parserPasswords =
    Parser.loop []
        (\list ->
            Parser.oneOf
                [ Parser.succeed (\password -> Parser.Loop (password :: list))
                    |= parserPassword
                    |. Parser.spaces
                , Parser.succeed ()
                    |> Parser.map (\_ -> Parser.Done (List.reverse list))
                ]
        )


parserPassword : Parser Password
parserPassword =
    Parser.succeed Password
        |. Parser.spaces
        |= Parser.int
        |. Parser.symbol "-"
        |. Parser.spaces
        |= Parser.int
        |. Parser.spaces
        |= parseSingleAlphaChar
        |. Parser.symbol ":"
        |. Parser.spaces
        |= parseAlphaString


parseSingleAlphaChar : Parser Char
parseSingleAlphaChar =
    Parser.succeed ()
        |. Parser.chompIf (\char -> Char.isAlpha char)
        |> Parser.getChompedString
        |> Parser.andThen
            (\a ->
                case String.uncons a of
                    Just ( char, _ ) ->
                        Parser.succeed char

                    _ ->
                        Parser.problem "Something went wrong parsing this char"
            )


parseAlphaString : Parser String
parseAlphaString =
    Parser.succeed ()
        |. Parser.chompWhile (\char -> Char.isAlpha char)
        |> Parser.getChompedString
