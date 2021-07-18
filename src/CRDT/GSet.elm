module CRDT.GSet exposing
    ( GSet
    , Operation(..)
    , apply
    , decoder
    , empty
    , encode
    , fromList
    , insert
    , member
    , merge
    , toList
    , toSet
    , patch
    )

{-| A GSet (or Grow-only Set) is a replicated set that can only be added to. Removing elements from the set is not supported.

# GSet

@docs GSet, empty, insert, member, merge, fromList, toList, toSet

# Operations

@docs Operation, apply, patch

# Serialization

@docs encode, decoder

-}

import Json.Decode
import Json.Encode
import Set


{-| GSet state.
-}
type GSet comparable
    = GSet (Set.Set comparable)


{-| Operations that will modify the state of the set.
-}
type Operation comparable
    = Insert comparable


{-| Constructor that creates a new empty GSet.
-}
empty : GSet comparable
empty =
    GSet Set.empty


{-| Insert a value in the set.
-}
insert : comparable -> GSet comparable -> GSet comparable
insert element (GSet set) =
    GSet (Set.insert element set)


{-| Determine if a value is in the set.
-}
member : comparable -> GSet comparable -> Bool
member element (GSet set) =
    Set.member element set


{-| Merge two GSet states.
-}
merge : GSet comparable -> GSet comparable -> GSet comparable
merge (GSet a) (GSet b) =
    GSet (Set.union a b)


{-| Apply an operation on a GSet.
-}
apply : Operation comparable -> GSet comparable -> GSet comparable
apply op set =
    case op of
        Insert element ->
            insert element set


{-|  Apply a list of operations (a patch) on a GSet.
-}
patch : List (Operation comparable) -> GSet comparable -> GSet comparable
patch ops counter =
    List.foldl apply counter ops


{-| Convert a list of values into a GSet.
-}
fromList : List comparable -> GSet comparable
fromList list =
    GSet (Set.fromList list)


{-| Convert a GSet to a list of values.
-}
toList : GSet comparable -> List comparable
toList (GSet set) =
    Set.toList set


{-| Convert a GSet to a Set.
-}
toSet : GSet comparable -> Set.Set comparable
toSet (GSet set) =
    set


{-| Encode a GSet as JSON.
-}
encode : (comparable -> Json.Encode.Value) -> GSet comparable -> Json.Encode.Value
encode encoder (GSet set) =
    Json.Encode.set encoder set


{-| Decode a GSet from JSON.
-}
decoder : Json.Decode.Decoder comparable -> Json.Decode.Decoder (GSet comparable)
decoder decode =
    Json.Decode.list decode
        |> Json.Decode.map Set.fromList
        |> Json.Decode.map GSet
