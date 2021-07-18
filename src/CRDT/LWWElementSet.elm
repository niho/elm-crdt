module CRDT.LWWElementSet exposing
    ( LWWElementSet
    , Operation(..)
    , apply
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
    , patch
    )

{-| An LWWElementSet (or Last-Write-Wins Element Set) is a set where each insert and remove operation is tagged with a timestamp and if an insert and remove occurs concurrently on two separate replicas, the operation with the latest timestamp will take precendence.

# LWWElementSet

@docs LWWElementSet, empty, insert, remove, member, merge, fromList, toList, toSet

# Operations

@docs Operation, apply, patch

# Serialization

@docs encode, decoder

-}

import Dict
import Json.Decode
import Json.Encode
import Set
import Time


{-| LWWElementSet state.
-}
type LWWElementSet comparable
    = LWWElementSet (Dict.Dict comparable Time.Posix) (Dict.Dict comparable Time.Posix)


{-| Operations that will modify the state of the set.
-}
type Operation comparable
    = Insert comparable Time.Posix
    | Remove comparable Time.Posix


{-| Constructor that creates a new empty GSet.
-}
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


{-| Insert a value in the set.
-}
insert : comparable -> Time.Posix -> LWWElementSet comparable -> LWWElementSet comparable
insert element now (LWWElementSet a r) =
    LWWElementSet (update element now a) r


{-| Remove a value from the set.
-}
remove : comparable -> Time.Posix -> LWWElementSet comparable -> LWWElementSet comparable
remove element now (LWWElementSet a r) =
    LWWElementSet a (update element now r)


{-| Determine if a value is in the set.
-}
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


{-| Merge two LWWElementSet states.
-}
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


{-| Apply an operation on a LWWElementSet.
-}
apply : Operation comparable -> LWWElementSet comparable -> LWWElementSet comparable
apply op set =
    case op of
        Insert element time ->
            insert element time set

        Remove element time ->
            remove element time set


{-|  Apply a list of operations (a patch) on a LWWElementSet.
-}
patch : List (Operation comparable) -> LWWElementSet comparable -> LWWElementSet comparable
patch ops set =
    List.foldl apply set ops


{-| Convert a list of values into a LWWElementSet.
-}
fromList : List comparable -> Time.Posix -> LWWElementSet comparable
fromList list now =
    LWWElementSet (Dict.fromList (List.map (\v -> ( v, now )) list)) Dict.empty


{-| Convert a LWWElementSet to a list of values.
-}
toList : LWWElementSet comparable -> List comparable
toList set =
    Set.toList (toSet set)


{-| Convert a LWWElementSet to a Set.
-}
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


{-| Encode a LWWElementSet as JSON.
-}
encode : LWWElementSet String -> Json.Encode.Value
encode (LWWElementSet a r) =
    Json.Encode.list
        (Json.Encode.dict identity (\t -> Json.Encode.int (Time.posixToMillis t)))
        [ a, r ]


{-| Decode a LWWElementSet from JSON.
-}
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
