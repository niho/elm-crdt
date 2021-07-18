module CRDT.PNCounter exposing
    ( PNCounter
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


type PNCounter comparable
    = PNCounter (GCounter comparable) (GCounter comparable)


type Op comparable
    = Increment comparable
    | Decrement comparable


zero : PNCounter comparable
zero =
    PNCounter CRDT.GCounter.zero CRDT.GCounter.zero


increment : comparable -> PNCounter comparable -> PNCounter comparable
increment id (PNCounter p n) =
    PNCounter (CRDT.GCounter.increment id p) n


decrement : comparable -> PNCounter comparable -> PNCounter comparable
decrement id (PNCounter p n) =
    PNCounter p (CRDT.GCounter.increment id n)


value : PNCounter comparable -> Int
value (PNCounter p n) =
    CRDT.GCounter.value p - CRDT.GCounter.value n


merge : PNCounter comparable -> PNCounter comparable -> PNCounter comparable
merge (PNCounter ap an) (PNCounter bp bn) =
    PNCounter
        (CRDT.GCounter.merge ap bp)
        (CRDT.GCounter.merge an bn)


apply : Op comparable -> PNCounter comparable -> PNCounter comparable
apply op counter =
    case op of
        Increment id ->
            increment id counter

        Decrement id ->
            decrement id counter


patch : List (Op comparable) -> PNCounter comparable -> PNCounter comparable
patch ops counter =
    List.foldl apply counter ops


encode : PNCounter String -> Json.Encode.Value
encode (PNCounter p n) =
    Json.Encode.list CRDT.GCounter.encode [ p, n ]


decoder : Json.Decode.Decoder (PNCounter String)
decoder =
    Json.Decode.map2 PNCounter
        (Json.Decode.index 0 CRDT.GCounter.decoder)
        (Json.Decode.index 0 CRDT.GCounter.decoder)
