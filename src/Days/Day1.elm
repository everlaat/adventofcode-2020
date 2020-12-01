module Days.Day1 exposing (part1, part2)


part1 : String -> Maybe Int
part1 =
    inputToList
        >> fold2
            (\a b r_ ->
                if r_ == Nothing && a + b == 2020 then
                    Just (a * b)

                else
                    r_
            )
            Nothing


part2 : String -> Maybe Int
part2 =
    inputToList
        >> fold3
            (\a b c r_ ->
                if r_ == Nothing && a + b + c == 2020 then
                    Just (a * b * c)

                else
                    r_
            )
            Nothing



--


inputToList : String -> List Int
inputToList =
    String.split "\n"
        >> List.filterMap (String.toInt << String.trim)


fold2 : (a -> a -> Maybe b -> Maybe b) -> Maybe b -> List a -> Maybe b
fold2 f x list =
    List.foldl
        (\a x_ ->
            List.foldl
                (\b x__ ->
                    if x__ == Nothing then
                        f a b x_

                    else
                        x__
                )
                x_
                list
        )
        x
        list


fold3 : (a -> a -> a -> Maybe b -> Maybe b) -> Maybe b -> List a -> Maybe b
fold3 f x list =
    List.foldl
        (\a x_ ->
            List.foldl
                (\b x__ ->
                    if x__ == Nothing then
                        List.foldl
                            (\c x___ ->
                                if x___ == Nothing then
                                    f a b c x_

                                else
                                    x___
                            )
                            x__
                            list

                    else
                        x__
                )
                x_
                list
        )
        x
        list
