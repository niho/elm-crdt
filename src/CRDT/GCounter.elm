module CRDT.GCounter exposing
    ( GCounter
    , Replica
    , Operation(..)
    , apply
    , decoder
    , encode
    , increment
    , merge
    , value
    , zero
    , patch
    )

{-| A GCounter (or Grow-only Counter) is a replicated counter that is guaranteed to always converge to a single value, despite concurrent updates. A GCounter only supports incrementing the counter. If you need a counter that can also be decremented, you should use a PNCounter instead.

# GCounter

@docs Replica, GCounter, zero, increment, value, merge

# Operations

@docs Operation, apply, patch

# Serialization

@docs encode, decoder

-}

import Dict exposing (Dict)
import Json.Decode
import Json.Encode


{-| Each replica that modifies the counter is represented by a unique string ID.
-}
type alias Replica =
    String


{-| GCounter state -}
type GCounter
    = GCounter (Dict Replica Int)


{-| Operations that will modify the state of the counter. A GCounter only supports a single operation (Increment).
-}
type Operation
    = Increment Replica


{-| Constructor that creates a new GCounter with the value zero.
-}
zero : GCounter
zero =
    GCounter Dict.empty


{-| Increment the GCounter (+1).
-}
increment : Replica -> GCounter -> GCounter
increment id (GCounter counter) =
    case Dict.get id counter of
        Just val ->
            GCounter (Dict.insert id (val + 1) counter)

        Nothing ->
            GCounter (Dict.insert id 1 counter)


{-| Get the current value of the GCounter.
-}
value : GCounter -> Int
value (GCounter counter) =
    Dict.foldl (\_ x sum -> sum + x) 0 counter


{-| Merge two GCounter states.
-}
merge : GCounter -> GCounter -> GCounter
merge (GCounter a) (GCounter b) =
    GCounter
        (Dict.merge
            (\k x result -> Dict.insert k x result)
            (\k a_ b_ result -> Dict.insert k (max a_ b_) result)
            (\k x result -> Dict.insert k x result)
            a
            b
            Dict.empty
        )


{-| Apply an operation on a GCounter.
-}
apply : Operation -> GCounter -> GCounter
apply op counter =
    case op of
        Increment id ->
            increment id counter


{-| Apply a list of operations (a patch) on a GCounter.
-}
patch : List Operation -> GCounter -> GCounter
patch ops counter =
    List.foldl apply counter ops


{-| Encode a GCounter as JSON.
-}
encode : GCounter -> Json.Encode.Value
encode (GCounter counter) =
    Json.Encode.dict identity Json.Encode.int counter


{-| Decode a GCounter from JSON.
-}
decoder : Json.Decode.Decoder GCounter
decoder =
    Json.Decode.dict Json.Decode.int
        |> Json.Decode.map GCounter
