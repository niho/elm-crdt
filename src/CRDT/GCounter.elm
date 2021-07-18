module CRDT.GCounter exposing
    ( GCounter
    , Replica
    , Op(..)
    , apply
    , decoder
    , encode
    , increment
    , merge
    , value
    , zero
    , patch
    )

import Dict exposing (Dict)
import Json.Decode
import Json.Encode


type alias Replica =
    String


type GCounter
    = GCounter (Dict Replica Int)


type Op
    = Increment Replica


zero : GCounter
zero =
    GCounter Dict.empty


increment : Replica -> GCounter -> GCounter
increment id (GCounter counter) =
    case Dict.get id counter of
        Just val ->
            GCounter (Dict.insert id (val + 1) counter)

        Nothing ->
            GCounter (Dict.insert id 1 counter)


value : GCounter -> Int
value (GCounter counter) =
    Dict.foldl (\_ x sum -> sum + x) 0 counter


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


apply : Op -> GCounter -> GCounter
apply op counter =
    case op of
        Increment id ->
            increment id counter


patch : List Op -> GCounter -> GCounter
patch ops counter =
    List.foldl apply counter ops


encode : GCounter -> Json.Encode.Value
encode (GCounter counter) =
    Json.Encode.dict identity Json.Encode.int counter


decoder : Json.Decode.Decoder GCounter
decoder =
    Json.Decode.dict Json.Decode.int
        |> Json.Decode.map GCounter
