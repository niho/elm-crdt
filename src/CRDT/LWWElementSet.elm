module CRDT.LWWElementSet exposing
    ( LWWElementSet
    , decoder
    , empty
    , encode
    , insert
    , member
    , merge
    , remove
    , toList
    , toSet
    )

import Json.Decode
import Json.Encode
import Set
import Time


type LWWElementSet comparable
    = LWWElementSet (List ( Time.Posix, comparable )) (List ( Time.Posix, comparable ))


empty : LWWElementSet comparable
empty =
    LWWElementSet [] []


insert : comparable -> Time.Posix -> LWWElementSet comparable -> LWWElementSet comparable
insert element now (LWWElementSet a r) =
    LWWElementSet (List.append [ ( now, element ) ] a) r


remove : comparable -> Time.Posix -> LWWElementSet comparable -> LWWElementSet comparable
remove element now (LWWElementSet a r) =
    LWWElementSet a (List.append [ ( now, element ) ] r)


member : comparable -> LWWElementSet comparable -> Bool
member element (LWWElementSet a r) =
    List.any (\( t, v ) -> v == element && not (List.any (\( t2, v2 ) -> v2 == element && Time.posixToMillis t2 > Time.posixToMillis t) r)) a


merge : LWWElementSet comparable -> LWWElementSet comparable -> LWWElementSet comparable
merge (LWWElementSet aa ar) (LWWElementSet ba br) =
    LWWElementSet
        (aa ++ ba)
        (ar ++ br)


toList : LWWElementSet comparable -> List comparable
toList set =
    Set.toList (toSet set)


toSet : LWWElementSet comparable -> Set.Set comparable
toSet (LWWElementSet a r) =
    List.foldl
        (\( t, v ) acc ->
            if member v (LWWElementSet a r) then
                Set.insert v acc

            else
                acc
        )
        Set.empty
        a


encode : LWWElementSet String -> Json.Encode.Value
encode (LWWElementSet a r) =
    Json.Encode.list
        (Json.Encode.list
            (\( t, v ) ->
                Json.Encode.list identity
                    [ Json.Encode.int (Time.posixToMillis t)
                    , Json.Encode.string v
                    ]
            )
        )
        [ a, r ]


decoder : Json.Decode.Decoder (LWWElementSet String)
decoder =
    let
        timestamp =
            Json.Decode.int
                |> Json.Decode.map Time.millisToPosix

        gset =
            Json.Decode.list
                (Json.Decode.map2 Tuple.pair
                    (Json.Decode.index 0 timestamp)
                    (Json.Decode.index 1 Json.Decode.string)
                )
    in
    Json.Decode.map2 LWWElementSet
        (Json.Decode.index 0 gset)
        (Json.Decode.index 1 gset)
