module CRDT.LWWElementSet exposing
    ( LWWElementSet
    , decoder
    , empty
    , encode
    , fromList
    , insert
    , member
    , merge
    , remove
    , toList
    , toSet
    )

import Dict
import Json.Decode
import Json.Encode
import Set
import Time


type LWWElementSet comparable
    = LWWElementSet (Dict.Dict comparable Time.Posix) (Dict.Dict comparable Time.Posix)


empty : LWWElementSet comparable
empty =
    LWWElementSet Dict.empty Dict.empty


update : comparable -> Time.Posix -> Dict.Dict comparable Time.Posix -> Dict.Dict comparable Time.Posix
update element now =
    Dict.update element
        (\t ->
            case t of
                Just tt ->
                    if Time.posixToMillis now > Time.posixToMillis tt then
                        Just now

                    else
                        t

                Nothing ->
                    Just now
        )


insert : comparable -> Time.Posix -> LWWElementSet comparable -> LWWElementSet comparable
insert element now (LWWElementSet a r) =
    LWWElementSet (update element now a) r


remove : comparable -> Time.Posix -> LWWElementSet comparable -> LWWElementSet comparable
remove element now (LWWElementSet a r) =
    LWWElementSet a (update element now r)


member : comparable -> LWWElementSet comparable -> Bool
member element (LWWElementSet a r) =
    case ( Dict.get element a, Dict.get element r ) of
        ( Just t, Nothing ) ->
            True

        ( Nothing, Just t ) ->
            False

        ( Just t1, Just t2 ) ->
            Time.posixToMillis t1 > Time.posixToMillis t2

        ( Nothing, Nothing ) ->
            False


merge : LWWElementSet comparable -> LWWElementSet comparable -> LWWElementSet comparable
merge (LWWElementSet aa ar) (LWWElementSet ba br) =
    let
        union a b =
            Dict.merge
                Dict.insert
                (\v t1 t2 acc ->
                    Dict.insert v
                        (Time.millisToPosix (max (Time.posixToMillis t1) (Time.posixToMillis t2)))
                        acc
                )
                Dict.insert
                a
                b
                Dict.empty
    in
    LWWElementSet
        (union aa ba)
        (union ar br)


fromList : List comparable -> Time.Posix -> LWWElementSet comparable
fromList list now =
    LWWElementSet (Dict.fromList (List.map (\v -> ( v, now )) list)) Dict.empty


toList : LWWElementSet comparable -> List comparable
toList set =
    Set.toList (toSet set)


toSet : LWWElementSet comparable -> Set.Set comparable
toSet (LWWElementSet a r) =
    Dict.foldl
        (\v t acc ->
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
        (Json.Encode.dict identity (\t -> Json.Encode.int (Time.posixToMillis t)))
        [ a, r ]


decoder : Json.Decode.Decoder (LWWElementSet String)
decoder =
    let
        timestamp =
            Json.Decode.int
                |> Json.Decode.map Time.millisToPosix
    in
    Json.Decode.map2 LWWElementSet
        (Json.Decode.index 0 (Json.Decode.dict timestamp))
        (Json.Decode.index 1 (Json.Decode.dict timestamp))
