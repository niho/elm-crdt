module CRDT.GSet exposing
    ( GSet
    , Op(..)
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

import Json.Decode
import Json.Encode
import Set


type GSet comparable
    = GSet (Set.Set comparable)


type Op comparable
    = Insert comparable


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


apply : Op comparable -> GSet comparable -> GSet comparable
apply op set =
    case op of
        Insert element ->
            insert element set


patch : List (Op comparable) -> GSet comparable -> GSet comparable
patch ops counter =
    List.foldl apply counter ops


fromList : List comparable -> GSet comparable
fromList list =
    GSet (Set.fromList list)


toList : GSet comparable -> List comparable
toList (GSet set) =
    Set.toList set


toSet : GSet comparable -> Set.Set comparable
toSet (GSet set) =
    set


encode : (comparable -> Json.Encode.Value) -> GSet comparable -> Json.Encode.Value
encode encoder (GSet set) =
    Json.Encode.set encoder set


decoder : Json.Decode.Decoder comparable -> Json.Decode.Decoder (GSet comparable)
decoder decode =
    Json.Decode.list decode
        |> Json.Decode.map Set.fromList
        |> Json.Decode.map GSet
