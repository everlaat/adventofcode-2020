module Days.Day1 exposing (part1, part2)


part1 : Int -> String -> Maybe Int
part1 target =
    inputToList
        >> filterFold2
            (\a b ->
                if a + b == target then
                    Just (a * b)

                else
                    Nothing
            )


part2 : Int -> String -> Maybe Int
part2 target =
    inputToList
        >> filterFold3
            (\a b c ->
                if a + b + c == target then
                    Just (a * b * c)

                else
                    Nothing
            )



--


inputToList : String -> List Int
inputToList =
    String.split "\n"
        >> List.filterMap (String.toInt << String.trim)


filterFold2 : (a -> a -> Maybe b) -> List a -> Maybe b
filterFold2 f list =
    List.foldl
        (\a x_ ->
            case x_ of
                Nothing ->
                    List.foldl
                        (\b x__ ->
                            case x__ of
                                Nothing ->
                                    f a b

                                Just _ ->
                                    x__
                        )
                        x_
                        list

                Just _ ->
                    x_
        )
        Nothing
        list


filterFold3 : (a -> a -> a -> Maybe b) -> List a -> Maybe b
filterFold3 f list =
    List.foldl
        (\a x_ ->
            case x_ of
                Nothing ->
                    List.foldl
                        (\b x__ ->
                            case x__ of
                                Nothing ->
                                    List.foldl
                                        (\c x___ ->
                                            case x___ of
                                                Nothing ->
                                                    f a b c

                                                Just _ ->
                                                    x___
                                        )
                                        x__
                                        list

                                Just _ ->
                                    x__
                        )
                        x_
                        list

                Just _ ->
                    x_
        )
        Nothing
        list
