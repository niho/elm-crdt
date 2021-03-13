module CRDT.GSet exposing
    ( GSet
    , decoder
    , empty
    , encode
    , insert
    , member
    , merge
    , toList
    , toSet
    )

import Json.Decode
import Json.Encode
import Set


type GSet comparable
    = GSet (Set.Set comparable)


empty : GSet comparable
empty =
    GSet Set.empty


insert : comparable -> GSet comparable -> GSet comparable
insert element (GSet set) =
    GSet (Set.insert element set)


member : comparable -> GSet comparable -> Bool
member element (GSet set) =
    Set.member element set


merge : GSet comparable -> GSet comparable -> GSet comparable
merge (GSet a) (GSet b) =
    GSet (Set.union a b)


toList : GSet comparable -> List comparable
toList (GSet set) =
    Set.toList set


toSet : GSet comparable -> Set.Set comparable
toSet (GSet set) =
    set


encode : GSet String -> Json.Encode.Value
encode (GSet set) =
    Json.Encode.set Json.Encode.string set


decoder : Json.Decode.Decoder (GSet String)
decoder =
    Json.Decode.list Json.Decode.string
        |> Json.Decode.map Set.fromList
        |> Json.Decode.map GSet
