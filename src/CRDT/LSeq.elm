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


append : a -> LSeq a -> LSeq a
append element (LSeq seq) =
    let
        p =
            leafId
                (Array.get (Array.length seq.sequence - 2) seq.sequence
                    |> Maybe.withDefault Start
                )

        q =
            leafId End
    in
    insert element p q (LSeq seq)


alloc : Id -> Id -> LSeq a -> ( ( Id, Random.Seed ), List Strategy )
alloc p q (LSeq seq) =
    let
        getLevel : Id -> Id -> Int -> Int -> ( Int, Int )
        getLevel p_ q_ depth_ interval_ =
            if List.sum interval_ < 1 && depth_ < 64 then
                getLevel p_ q_ (depth_ + 1) (idiff (prefix q_ depth_) (prefix p_ depth_))

            else
                ( depth_, interval_ )

        idiff : Id -> Id -> Id
        idiff p_ q_ =
            [ List.sum p_ - List.sum q_ - 1 ]

        prefix id depth_ =
            List.take depth_ id

        ( depth, interval ) =
            getInterval p q 0 0

        step =
            min seq.boundary interval

        randomStrategy =
            Random.uniform BPlus [ BMinus ]

        getStrategy : Int -> Random.Seed -> List Strategy -> ( ( Strategy, Random.Seed ), List Strategy )
        getStrategy depth_ seed s =
            case s of
                [] ->
                    let
                        ( s_, seed_ ) =
                            Random.step randomStrategy seed
                    in
                    ( ( s_, seed_ ), s ++ [ s_ ] )

                s_ :: rest ->
                    if depth_ == 0 then
                        ( ( s_, seed ), s )

                    else
                        getStrategy (depth_ - 1) seed rest

        bPlus p_ depth_ step_ =
            Random.int 0 step_
                |> Random.map (\i -> (prefix p_ depth_) ++ [ i + 1 ])
                |> Random.step

        bMinus q_ depth_ step_ =
            Random.int 0 step_
                |> Random.map (\i -> (prefix q_ depth_) ++ [ i + 1 ])
                |> Random.step
    in
    case getStrategy depth seq.seed seq.strategy of
        ( ( BPlus, seed ), strategy_ ) ->
            ( bPlus p depth step seed, strategy_ )

        ( ( BMinus, seed ), strategy_ ) ->
            ( bMinus q depth step seed, strategy_ )


insert : a -> Id -> Id -> LSeq a -> LSeq a
insert element p q (LSeq seq) =
    let
        ( ( id, seed ), strategy ) =
            alloc p q (LSeq seq)

        compareId a b =
            case ( a, b ) of
                ( [], [] ) ->
                    EQ

                ( [], h :: rest ) ->
                    LT

                ( h :: rest, [] ) ->
                    GT

                ( h1 :: rest1, h2 :: rest2 ) ->
                    case compare h1 h2 of
                        EQ ->
                            compareId rest1 rest2

                        LT ->
                            LT

                        GT ->
                            GT

        compareLeaf a b =
            compareId (leafId a) (leafId b)

        applyInsert value xs =
            xs
                |> Array.push value
                |> Array.toList
                |> List.sortWith compareLeaf
                |> Array.fromList
    in
    LSeq
        { seq
            | sequence = applyInsert (Value ( element, id )) seq.sequence
            , counter = seq.counter + 1
            , strategy = strategy
            , seed = seed
        }


dropRight : Int -> LSeq a -> LSeq a
dropRight n (LSeq seq) =
    if n > 0 then
        case Array.get (Array.length seq.sequence - 1) seq.sequence of
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
