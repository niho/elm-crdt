module CRDT.PNCounter exposing
    ( PNCounter
    , decoder
    , decrement
    , encode
    , increment
    , merge
    , query
    , zero
    )

import CRDT.GCounter exposing (GCounter)
import Json.Decode
import Json.Encode


type PNCounter comparable
    = PNCounter (GCounter comparable) (GCounter comparable)


zero : PNCounter comparable
zero =
    PNCounter CRDT.GCounter.zero CRDT.GCounter.zero


increment : comparable -> PNCounter comparable -> PNCounter comparable
increment id (PNCounter p n) =
    PNCounter (CRDT.GCounter.increment id p) n


decrement : comparable -> PNCounter comparable -> PNCounter comparable
decrement id (PNCounter p n) =
    PNCounter p (CRDT.GCounter.increment id n)


query : PNCounter comparable -> Int
query (PNCounter p n) =
    CRDT.GCounter.query p - CRDT.GCounter.query n


merge : PNCounter comparable -> PNCounter comparable -> PNCounter comparable
merge (PNCounter ap an) (PNCounter bp bn) =
    PNCounter
        (CRDT.GCounter.merge ap bp)
        (CRDT.GCounter.merge an bn)


encode : PNCounter String -> Json.Encode.Value
encode (PNCounter p n) =
    Json.Encode.list CRDT.GCounter.encode [ p, n ]


decoder : Json.Decode.Decoder (PNCounter String)
decoder =
    Json.Decode.map2 PNCounter
        (Json.Decode.index 0 CRDT.GCounter.decoder)
        (Json.Decode.index 0 CRDT.GCounter.decoder)
