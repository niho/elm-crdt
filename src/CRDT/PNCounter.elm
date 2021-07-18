module CRDT.PNCounter exposing
    ( PNCounter
    , Replica
    , Op(..)
    , apply
    , decoder
    , decrement
    , encode
    , increment
    , merge
    , value
    , zero
    , patch
    )

{-| A PNCounter (or Positive-Negative Counter) is a replicated counter that can be both incremented and decremented.

# PNCounter

@docs Replica, PNCounter, zero, increment, decrement, value, merge

# Operations

@docs Op, apply, patch

# Serialization

@docs encode, decoder

-}

import CRDT.GCounter exposing (GCounter)
import Json.Decode
import Json.Encode


{-| Each replica that modifies the counter is represented by a unique string ID.
-}
type alias Replica =
    CRDT.GCounter.Replica


{-| PNCounter state -}
type PNCounter
    = PNCounter GCounter GCounter


{-| Operations that will modify the state of the counter.
-}
type Op
    = Increment Replica
    | Decrement Replica


{-| Constructor that creates a new PNCounter with the value zero.
-}
zero : PNCounter
zero =
    PNCounter CRDT.GCounter.zero CRDT.GCounter.zero


{-| Increment the PNCounter.
-}
increment : Replica -> PNCounter -> PNCounter
increment id (PNCounter p n) =
    PNCounter (CRDT.GCounter.increment id p) n


{-| Decrement the PNCounter.
-}
decrement : Replica -> PNCounter -> PNCounter
decrement id (PNCounter p n) =
    PNCounter p (CRDT.GCounter.increment id n)


{-| Get the current value of the PNCounter.
-}
value : PNCounter -> Int
value (PNCounter p n) =
    CRDT.GCounter.value p - CRDT.GCounter.value n


{-| Merge two PNCounter states.
-}
merge : PNCounter -> PNCounter -> PNCounter
merge (PNCounter ap an) (PNCounter bp bn) =
    PNCounter
        (CRDT.GCounter.merge ap bp)
        (CRDT.GCounter.merge an bn)


{-| Apply an operation on a PNCounter.
-}
apply : Op -> PNCounter -> PNCounter
apply op counter =
    case op of
        Increment id ->
            increment id counter

        Decrement id ->
            decrement id counter


{-| Apply a list of operations (a patch) on a PNCounter.
-}
patch : List Op -> PNCounter -> PNCounter
patch ops counter =
    List.foldl apply counter ops


{-| Encode a PNCounter as JSON.
-}
encode : PNCounter -> Json.Encode.Value
encode (PNCounter p n) =
    Json.Encode.list CRDT.GCounter.encode [ p, n ]


{-| Decode a PNCounter from JSON.
-}
decoder : Json.Decode.Decoder PNCounter
decoder =
    Json.Decode.map2 PNCounter
        (Json.Decode.index 0 CRDT.GCounter.decoder)
        (Json.Decode.index 0 CRDT.GCounter.decoder)
