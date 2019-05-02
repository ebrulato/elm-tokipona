module Tools exposing (cutList, firstPos, zip)


firstPos : a -> List a -> Int
firstPos v l =
    let
        ( p, _ ) =
            List.foldl
                (\tmp ( fPos, pos ) ->
                    if fPos == 0 && tmp == v then
                        ( pos + 1, pos + 1 )

                    else
                        ( fPos, pos + 1 )
                )
                ( 0, 0 )
                l
    in
    p


cutList : (a -> Bool) -> Bool -> Bool -> List a -> ( List a, List a, Int )
cutList f keep after l =
    let
        ( idx, pos ) =
            List.foldl
                (\item ( i, p ) ->
                    if f item && (i == -1) then
                        ( p, p + 1 )

                    else
                        ( i, p + 1 )
                )
                ( -1, 1 )
                l
    in
    if idx > 0 then
        if keep then
            if after then
                ( List.take (idx - 1) l, List.drop (idx - 1) l, idx )

            else
                ( List.take idx l, List.drop idx l, idx )

        else
            ( List.take (idx - 1) l, List.drop idx l, idx )

    else
        ( [], l, 0 )


zip : List a -> List a -> List ( a, a )
zip l1 l2 =
    let
        h1 =
            List.head l1

        h2 =
            List.head l2

        t1 =
            List.tail l1

        t2 =
            List.tail l2
    in
    case h1 of
        Just h1v ->
            case h2 of
                Just h2v ->
                    case t1 of
                        Just t1v ->
                            case t2 of
                                Just t2v ->
                                    ( h1v, h2v ) :: zip t1v t2v

                                Nothing ->
                                    ( h1v, h2v ) :: []

                        Nothing ->
                            ( h1v, h2v ) :: []

                Nothing ->
                    []

        Nothing ->
            []
