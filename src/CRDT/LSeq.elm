module CRDT.LSeq exposing
    ( LSeq
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
import CRDT.LSeq.Position exposing (..)
import Random


type Leaf a
    = Value ( a, Id )
    | Tombstone ( a, Id )
    | Start
    | End


type Strategy
    = BPlus
    | BMinus


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
        , base = 16
        , seed = seed
        }


length : LSeq a -> Int
length seq =
    List.length (toList seq)


leafId : Int -> Leaf a -> Id
leafId base leaf =
    case leaf of
        Value ( _, id ) ->
            id

        Start ->
            --Id 0 0
            minId

        End ->
            --Id ((base * (2 ^ 1)) - 1) 0
            maxId

        Tombstone ( _, id ) ->
            id


append : a -> LSeq a -> LSeq a
append element (LSeq seq) =
    let
        p =
            leafId seq.base
                (Array.get (Array.length seq.sequence - 2) seq.sequence
                    |> Maybe.withDefault Start
                )

        q =
            leafId seq.base End
    in
    insert element p q (LSeq seq)


alloc : Id -> Id -> LSeq a -> ( ( Id, Random.Seed ), List Strategy )
alloc p q (LSeq seq) =
    let
        getInterval p_ q_ depth_ interval_ =
            if interval_ < 1 then
                if Debug.log "depth" depth_ > 64 then
                    Debug.log "infinite loop"
                        ( depth_, interval_ )

                else
                    getInterval p_
                        q_
                        (depth_ + 1)
                        (Debug.log "interval" <| diff p_ q_ depth_ - 1)
                {- ((Debug.log "q-prefix" <| prefix q_ depth_ seq.base)
                       - (Debug.log "p-prefix" <| prefix p_ depth_ seq.base)
                       - 1
                   )
                -}

            else
                Debug.log "result" ( depth_ - 1, interval_ )

        ( depth, interval ) =
            getInterval (Debug.log "p" p) (Debug.log "q" q) 1 0

        step =
            min seq.boundary interval

        randomStrategy =
            Random.uniform BPlus [ BMinus ]

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
                |> Random.map (\i -> add ( p_, depth_ ) (i + 1))
                |> Random.step

        bMinus q_ depth_ step_ =
            Random.int 0 step_
                |> Random.map (\i -> sub ( q_, depth_ ) (i + 1))
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
            Debug.log "alloc" <| alloc p q (LSeq seq)

        {- compareId (Id a l1) (Id b l2) =
           compare
               (Debug.log "a" <| prefix a (max l1 l2) seq.base)
               (Debug.log "b" <| prefix b (max l1 l2) seq.base)
        -}
        compareLeaf a b =
            compareId (leafId seq.base a) (leafId seq.base b)

        applyInsert value xs =
            xs
                |> Array.push (Debug.log "value" value)
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
                remove (leafId seq.base leaf) (dropRight (n - 1) (LSeq seq))

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
