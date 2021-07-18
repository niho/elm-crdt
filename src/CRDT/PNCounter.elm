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

import CRDT.GCounter exposing (GCounter)
import Json.Decode
import Json.Encode


type alias Replica =
    CRDT.GCounter.Replica


type PNCounter
    = PNCounter GCounter GCounter


type Op
    = Increment Replica
    | Decrement Replica


zero : PNCounter
zero =
    PNCounter CRDT.GCounter.zero CRDT.GCounter.zero


increment : Replica -> PNCounter -> PNCounter
increment id (PNCounter p n) =
    PNCounter (CRDT.GCounter.increment id p) n


decrement : Replica -> PNCounter -> PNCounter
decrement id (PNCounter p n) =
    PNCounter p (CRDT.GCounter.increment id n)


value : PNCounter -> Int
value (PNCounter p n) =
    CRDT.GCounter.value p - CRDT.GCounter.value n


merge : PNCounter -> PNCounter -> PNCounter
merge (PNCounter ap an) (PNCounter bp bn) =
    PNCounter
        (CRDT.GCounter.merge ap bp)
        (CRDT.GCounter.merge an bn)


apply : Op -> PNCounter -> PNCounter
apply op counter =
    case op of
        Increment id ->
            increment id counter

        Decrement id ->
            decrement id counter


patch : List Op -> PNCounter -> PNCounter
patch ops counter =
    List.foldl apply counter ops


encode : PNCounter -> Json.Encode.Value
encode (PNCounter p n) =
    Json.Encode.list CRDT.GCounter.encode [ p, n ]


decoder : Json.Decode.Decoder PNCounter
decoder =
    Json.Decode.map2 PNCounter
        (Json.Decode.index 0 CRDT.GCounter.decoder)
        (Json.Decode.index 0 CRDT.GCounter.decoder)
