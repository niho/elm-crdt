module CRDT.TwoPSet exposing
    ( TwoPSet
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

{-| A TwoPSet is a replicated set in which removals take precedence over additions. For example, if one replica removes and re-adds an element, while another replica concurrently removes the element, then the merged outcome is that the element is not in the set.

# TwoPSet

@docs TwoPSet, empty, insert, remove, member, merge

# Operations

@docs Operation, apply, patch

# Serialization

@docs encode, decoder

-}

import CRDT.GSet exposing (GSet)
import Json.Decode
import Json.Encode
import Set


{-| TwoPSet state.
-}
type TwoPSet comparable
    = TwoPSet (GSet comparable) (GSet comparable)


{-| Operations that will modify the state of the set.
-}
type Operation comparable
    = Insert comparable
    | Remove comparable


{-| Constructor that creates a new empty TwoPSet.
-}
empty : TwoPSet comparable
empty =
    TwoPSet CRDT.GSet.empty CRDT.GSet.empty


{-| Insert a value in the set.
-}
insert : comparable -> TwoPSet comparable -> TwoPSet comparable
insert element (TwoPSet a r) =
    TwoPSet (CRDT.GSet.insert element a) r


{-| Remove a value from the set.
-}
remove : comparable -> TwoPSet comparable -> TwoPSet comparable
remove element (TwoPSet a r) =
    case CRDT.GSet.member element a of
        True ->
            TwoPSet a (CRDT.GSet.insert element r)

        False ->
            TwoPSet a r


{-| Determine if a value is in the set.
-}
member : comparable -> TwoPSet comparable -> Bool
member element (TwoPSet a r) =
    CRDT.GSet.member element a && not (CRDT.GSet.member element r)


{-| Merge two TwoPSet states.
-}
merge : TwoPSet comparable -> TwoPSet comparable -> TwoPSet comparable
merge (TwoPSet aa ar) (TwoPSet ba br) =
    TwoPSet
        (CRDT.GSet.merge aa ba)
        (CRDT.GSet.merge ar br)


{-| Apply an operation on a TwoPSet.
-}
apply : Operation comparable -> TwoPSet comparable -> TwoPSet comparable
apply op set =
    case op of
        Insert element ->
            insert element set

        Remove element ->
            remove element set


{-|  Apply a list of operations (a patch) on a TwoPSet.
-}
patch : List (Operation comparable) -> TwoPSet comparable -> TwoPSet comparable
patch ops set =
    List.foldl apply set ops


{-| Convert a list of values into a TwoPSet.
-}
fromList : List comparable -> TwoPSet comparable
fromList list =
    TwoPSet (CRDT.GSet.fromList list) CRDT.GSet.empty


{-| Convert a TwoPSet to a list of values.
-}
toList : TwoPSet comparable -> List comparable
toList set =
    Set.toList (toSet set)


{-| Convert a TwoPSet to a Set.
-}
toSet : TwoPSet comparable -> Set.Set comparable
toSet (TwoPSet a r) =
    Set.diff (CRDT.GSet.toSet a) (CRDT.GSet.toSet r)


{-| Encode a TwoPSet as JSON.
-}
encode : (comparable -> Json.Encode.Value) -> TwoPSet comparable -> Json.Encode.Value
encode encoder (TwoPSet a r) =
    Json.Encode.list
        (CRDT.GSet.encode encoder)
        [ a, r ]


{-| Decode a TwoPSet from JSON.
-}
decoder : Json.Decode.Decoder comparable -> Json.Decode.Decoder (TwoPSet comparable)
decoder decode =
    Json.Decode.map2 TwoPSet
        (Json.Decode.index 0 (CRDT.GSet.decoder decode))
        (Json.Decode.index 1 (CRDT.GSet.decoder decode))
        |> Json.Decode.andThen
            (\(TwoPSet a r) ->
                if Set.intersect (CRDT.GSet.toSet a) (CRDT.GSet.toSet r) == CRDT.GSet.toSet r then
                    Json.Decode.succeed (TwoPSet a r)

                else
                    Json.Decode.fail "inconsistent TwoPSet state"
            )
