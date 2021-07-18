module CRDT.ORSet exposing
    ( ORSet
    , Replica
    , Op(..)
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

{-| An ORSet (Observed Remove Set) is a replicated set in which additions take precedence over removals. For example, if one replica removes and re-adds an element, while another replica concurrently removes the element, then the merged outcome is that the element is in the set.

# ORSet

@docs Replica, ORSet, empty, insert, remove, member, merge

# Operations

@docs Op, apply, patch

# Serialization

@docs encode, decoder

-} 

import Dict
import Json.Decode
import Json.Encode
import Set
import Time


{-| Each replica that modifies the set is represented by a unique string ID.
-}
type alias Replica =
    String


type alias Tags =
    Set.Set Replica


{-| ORSet state.
-}
type ORSet comparable
    = ORSet (Dict.Dict comparable Tags) (Dict.Dict comparable Tags)


{-| Operations that will modify the state of the set.
-}
type Op comparable
    = Insert comparable Replica
    | Remove comparable


{-| Constructor that creates a new empty ORSet.
-}
empty : ORSet comparable
empty =
    ORSet Dict.empty Dict.empty


{-| Insert a value in the set.
-}
insert : comparable -> Replica -> ORSet comparable -> ORSet comparable
insert element tag (ORSet a r) =
    let
        update tags =
            Just <|
                case tags of
                    Just tags_ ->
                        Set.insert tag tags_

                    Nothing ->
                        Set.singleton tag
    in
    ORSet (Dict.update element update a) r


{-| Remove a value from the set.
-}
remove : comparable -> ORSet comparable -> ORSet comparable
remove element (ORSet a r) =
    ORSet a (Dict.update element (\_ -> Dict.get element a) r)


{-| Determine if a value is in the set.
-}
member : comparable -> ORSet comparable -> Bool
member element (ORSet a r) =
    case ( Dict.get element a, Dict.get element r ) of
        ( Just t, Nothing ) ->
            True

        ( Nothing, Just t ) ->
            False

        ( Just t1, Just t2 ) ->
            Set.diff t1 t2 |> Set.isEmpty |> not

        ( Nothing, Nothing ) ->
            False


{-| Merge two ORSet states.
-}
merge : ORSet comparable -> ORSet comparable -> ORSet comparable
merge (ORSet aa ar) (ORSet ba br) =
    let
        union a b =
            Dict.merge
                Dict.insert
                (\v t1 t2 acc ->
                    Dict.insert v (Set.union t1 t2) acc
                )
                Dict.insert
                a
                b
                Dict.empty
    in
    ORSet
        (union aa ba)
        (union ar br)


{-| Apply an operation on an ORSet.
-}
apply : Op comparable -> ORSet comparable -> ORSet comparable
apply op set =
    case op of
        Insert element tag ->
            insert element tag set

        Remove element ->
            remove element set


{-|  Apply a list of operations (a patch) on an ORSet.
-}
patch : List (Op comparable) -> ORSet comparable -> ORSet comparable
patch ops set =
    List.foldl apply set ops


{-| Convert a list of values into an ORSet.
-}
fromList : List comparable -> String -> ORSet comparable
fromList list tag =
    ORSet (Dict.fromList (List.map (\v -> ( v, Set.singleton tag )) list)) Dict.empty


{-| Convert an ORSet to a list of values.
-}
toList : ORSet comparable -> List comparable
toList set =
    Set.toList (toSet set)


{-| Convert an ORSet to a Set.
-}
toSet : ORSet comparable -> Set.Set comparable
toSet (ORSet a r) =
    Dict.foldl
        (\v t acc ->
            if member v (ORSet a r) then
                Set.insert v acc

            else
                acc
        )
        Set.empty
        a


{-| Encode an ORSet as JSON.
-}
encode : ORSet String -> Json.Encode.Value
encode (ORSet a r) =
    Json.Encode.list
        (Json.Encode.dict identity (Json.Encode.set Json.Encode.string))
        [ a, r ]


{-| Decode an ORSet from JSON.
-}
decoder : Json.Decode.Decoder (ORSet String)
decoder =
    let
        tags =
            Json.Decode.list Json.Decode.string
                |> Json.Decode.map Set.fromList
    in
    Json.Decode.map2 ORSet
        (Json.Decode.index 0 (Json.Decode.dict tags))
        (Json.Decode.index 1 (Json.Decode.dict tags))
