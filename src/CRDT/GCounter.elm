module CRDT.GCounter exposing
    ( GCounter
    , decoder
    , encode
    , increment
    , merge
    , query
    , zero
    )

import Dict exposing (Dict)
import Json.Decode
import Json.Encode


type GCounter comparable
    = GCounter (Dict comparable Int)


zero : GCounter comparable
zero =
    GCounter Dict.empty


increment : comparable -> GCounter comparable -> GCounter comparable
increment id (GCounter counter) =
    case Dict.get id counter of
        Just value ->
            GCounter (Dict.insert id (value + 1) counter)

        Nothing ->
            GCounter (Dict.insert id 1 counter)


query : GCounter comparable -> Int
query (GCounter counter) =
    Dict.foldl (\_ x sum -> sum + x) 0 counter


merge : GCounter comparable -> GCounter comparable -> GCounter comparable
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


encode : GCounter String -> Json.Encode.Value
encode (GCounter counter) =
    Json.Encode.dict identity Json.Encode.int counter


decoder : Json.Decode.Decoder (GCounter String)
decoder =
    Json.Decode.dict Json.Decode.int
        |> Json.Decode.map GCounter
