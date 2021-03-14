module CRDT.LSeq exposing
    ( Id
    , LSeq
    , append
    , dropRight
    , empty
    , fromList
    , fromString
    , insert
    , length
    , merge
    , remove
    , toList
    , toString
    )

import Array
import Random


type alias Id =
    List Int


type Strategy
    = BPlus
    | BMinus


type Leaf a
    = Value ( a, Id )
    | Tombstone ( a, Id )
    | TombstoneUnkown Id
    | Start
    | End


type LSeq a
    = LSeq
        { sequence : Array.Array (Leaf a)
        , strategy : List Strategy
        , boundary : Int
        , counter : Int
        , base : Int
        , seed : Random.Seed
        }


empty : Random.Seed -> LSeq a
empty seed =
    LSeq
        { sequence = Array.fromList [ Start, End ]
        , strategy = []
        , boundary = 10
        , counter = 0
        , base = 3
        , seed = seed
        }


length : LSeq a -> Int
length seq =
    List.length (toList seq)


leafId : Leaf a -> Id
leafId leaf =
    case leaf of
        Value ( _, id ) ->
            id

        Start ->
            [ 0 ]

        End ->
            [ 99 ]

        Tombstone ( _, id ) ->
            id

        TombstoneUnkown id ->
            id


append : a -> LSeq a -> LSeq a
append element (LSeq seq) =
    let
        p =
            leafId (Array.get -2 seq.sequence |> Maybe.withDefault Start)

        q =
            leafId End
    in
    insert element p q (LSeq seq)


insert : a -> Id -> Id -> LSeq a -> LSeq a
insert element p q (LSeq seq) =
    let
        randomPath =
            Random.int 0 99
                |> Random.map (\i -> p ++ [ i ])

        alloc =
            Random.step randomPath seq.seed

        ( id, seed ) =
            alloc

        splitAt idx arr =
            ( Array.slice 0 idx arr
            , Array.slice idx -1 arr
            )

        binaryIndexOf id_ xs =
            binsearch (Array.map leafId xs) id_ 0 (Array.length xs - 1)

        binsearch xs value low high =
            let
                mid =
                    low + (truncate ((high - low) / 2))
            in
            if high < low then
                Nothing

            else if mid > value then
                binsearch xs value low (mid - 1)

            else if mid < value then
                binsearch xs value (mid + 1) high

            else
                Just mid

        applyInsert xs =
            case binaryIndexOf id xs of
                Just index ->
                    let
                        ( ys, zs ) =
                            splitAt index xs
                    in
                    ys |> Array.push (Value ( element, id )) |> Array.append zs

                Nothing ->
                    xs |> Array.push (Value ( element, id ))
    in
    LSeq
        { seq
            | sequence = applyInsert seq.sequence
            , counter = seq.counter + 1
            , strategy = seq.strategy
            , seed = seed
        }


dropRight : Int -> LSeq a -> LSeq a
dropRight n (LSeq seq) =
    if n > 0 then
        case Array.get -1 seq.sequence of
            Just leaf ->
                remove (leafId leaf) (dropRight (n - 1) (LSeq seq))

            Nothing ->
                LSeq seq

    else
        LSeq seq


remove : Id -> LSeq a -> LSeq a
remove id (LSeq seq) =
    let
        applyRemove =
            Array.map
                (\leaf ->
                    case leaf of
                        Value ( el, id_ ) ->
                            if id == id_ then
                                Tombstone ( el, id )

                            else
                                Value ( el, id )

                        _ ->
                            leaf
                )
    in
    LSeq { seq | sequence = applyRemove seq.sequence }


toList : LSeq a -> List ( a, Id )
toList (LSeq seq) =
    seq.sequence
        |> Array.toList
        |> List.filterMap
            (\leaf ->
                case leaf of
                    Value el ->
                        Just el

                    _ ->
                        Nothing
            )


fromList : Random.Seed -> List a -> LSeq a
fromList seed =
    List.foldl append (empty seed)


toString : LSeq Char -> String
toString seq =
    String.fromList (toList seq |> List.map Tuple.first)


fromString : Random.Seed -> String -> LSeq Char
fromString seed str =
    str |> String.toList |> fromList seed


merge : LSeq a -> LSeq a -> LSeq a
merge (LSeq a) (LSeq b) =
    LSeq
        { sequence = Array.append a.sequence b.sequence
        , strategy = a.strategy ++ b.strategy
        , boundary = max a.boundary b.boundary
        , counter = max a.counter b.counter
        , base = max a.base b.base
        , seed = a.seed
        }
