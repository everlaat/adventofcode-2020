module Solvers.Y2020.Day4 exposing (part1, part2, partToSolver, solvers)

import Com.Solver as Solver exposing (Solver)
import Hex
import Html exposing (a)


type alias Passport =
    { byr : Maybe String
    , iyr : Maybe String
    , eyr : Maybe String
    , hgt : Maybe String
    , hcl : Maybe String
    , ecl : Maybe String
    , pid : Maybe String
    , cid : Maybe String
    }


emptyPassport : Passport
emptyPassport =
    { byr = Nothing
    , iyr = Nothing
    , eyr = Nothing
    , hgt = Nothing
    , hcl = Nothing
    , ecl = Nothing
    , pid = Nothing
    , cid = Nothing
    }


solvers : List Solver
solvers =
    [ Solver.make 2020 4 1 (part1 |> partToSolver)
    , Solver.make 2020 4 2 (part2 |> partToSolver)
    ]


inputToListOfPassport : String -> List Passport
inputToListOfPassport =
    String.trim
        >> String.lines
        >> List.foldl
            (\a b ->
                if String.trim a == "" then
                    "" :: b

                else
                    case b of
                        [] ->
                            a :: b

                        head :: rest ->
                            (a ++ " " ++ head) :: rest
            )
            []
        >> List.map
            (String.split " "
                >> List.foldl
                    (\a b ->
                        case String.trim a |> String.split ":" of
                            [ "byr", v ] ->
                                { b | byr = Just v }

                            [ "iyr", v ] ->
                                { b | iyr = Just v }

                            [ "eyr", v ] ->
                                { b | eyr = Just v }

                            [ "hgt", v ] ->
                                { b | hgt = Just v }

                            [ "hcl", v ] ->
                                { b | hcl = Just v }

                            [ "ecl", v ] ->
                                { b | ecl = Just v }

                            [ "pid", v ] ->
                                { b | pid = Just v }

                            [ "cid", v ] ->
                                { b | cid = Just v }

                            _ ->
                                b
                    )
                    emptyPassport
            )



-- >> Debug.log "?"
-- >> (\a ->
--         let
--             _ =
--                 Debug.log "passports: " (List.length a)
--         in
--         a
--    )


partToSolver : (List Passport -> String) -> (String -> Result String String)
partToSolver f =
    inputToListOfPassport >> f >> Ok


part1 : List Passport -> String
part1 =
    List.filter isValidSimple
        >> List.length
        >> String.fromInt


isValidSimple : Passport -> Bool
isValidSimple { byr, iyr, eyr, hgt, hcl, ecl, pid } =
    byr /= Nothing && iyr /= Nothing && eyr /= Nothing && hgt /= Nothing && hcl /= Nothing && ecl /= Nothing && pid /= Nothing


part2 : List Passport -> String
part2 =
    List.filter isValid
        >> List.length
        >> String.fromInt


isValid : Passport -> Bool
isValid { byr, iyr, eyr, hgt, hcl, ecl, pid } =
    [ Maybe.map isValid_byr byr |> Maybe.withDefault False
    , Maybe.map isValid_iyr iyr |> Maybe.withDefault False
    , Maybe.map isValid_eyr eyr |> Maybe.withDefault False
    , Maybe.map isValid_hgt hgt |> Maybe.withDefault False
    , Maybe.map isValid_hcl hcl |> Maybe.withDefault False
    , Maybe.map isValid_ecl ecl |> Maybe.withDefault False
    , Maybe.map isValid_pid pid |> Maybe.withDefault False
    ]
        |> List.all identity


isValid_byr : String -> Bool
isValid_byr str =
    case String.toInt str of
        Just int ->
            int >= 1920 && int <= 2020

        Nothing ->
            False


isValid_iyr : String -> Bool
isValid_iyr str =
    case String.toInt str of
        Just int ->
            int >= 2010 && int <= 2020

        Nothing ->
            False


isValid_eyr : String -> Bool
isValid_eyr str =
    case String.toInt str of
        Just int ->
            int >= 2020 && int <= 2030

        Nothing ->
            False


isValid_hgt : String -> Bool
isValid_hgt str =
    if String.contains "cm" str || String.contains "in" str then
        case String.split "cm" str |> List.head |> Maybe.andThen String.toInt of
            Just int ->
                int >= 150 && int <= 193

            Nothing ->
                case String.split "in" str |> List.head |> Maybe.andThen String.toInt of
                    Just int ->
                        int >= 59 && int <= 76

                    Nothing ->
                        False

    else
        False


isValid_hcl : String -> Bool
isValid_hcl str =
    if String.contains "#" str then
        String.replace "#" "" str
            |> Hex.fromString
            |> Result.toMaybe
            |> (/=) Nothing

    else
        False


isValid_ecl : String -> Bool
isValid_ecl str =
    List.member str [ "amb", "blu", "brn", "gry", "grn", "hzl", "oth" ]


isValid_pid : String -> Bool
isValid_pid str =
    String.toList str
        |> List.map Char.isDigit
        |> (\a -> List.all identity a && List.length a == 9)
