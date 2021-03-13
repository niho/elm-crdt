module CRDT.TwoPSet exposing
    ( TwoPSet
    , decoder
    , empty
    , encode
    , insert
    , member
    , merge
    , remove
    , toList
    , toSet
    )

import CRDT.GSet exposing (GSet)
import Json.Decode
import Json.Encode
import Set


type TwoPSet comparable
    = TwoPSet (GSet comparable) (GSet comparable)


empty : TwoPSet comparable
empty =
    TwoPSet CRDT.GSet.empty CRDT.GSet.empty


insert : comparable -> TwoPSet comparable -> TwoPSet comparable
insert element (TwoPSet a r) =
    TwoPSet (CRDT.GSet.insert element a) r


remove : comparable -> TwoPSet comparable -> TwoPSet comparable
remove element (TwoPSet a r) =
    case CRDT.GSet.member element a of
        True ->
            TwoPSet a (CRDT.GSet.insert element r)

        False ->
            TwoPSet a r


member : comparable -> TwoPSet comparable -> Bool
member element (TwoPSet a r) =
    CRDT.GSet.member element a && not (CRDT.GSet.member element r)


merge : TwoPSet comparable -> TwoPSet comparable -> TwoPSet comparable
merge (TwoPSet aa ar) (TwoPSet ba br) =
    TwoPSet
        (CRDT.GSet.merge aa ba)
        (CRDT.GSet.merge ar br)


toList : TwoPSet comparable -> List comparable
toList set =
    Set.toList (toSet set)


toSet : TwoPSet comparable -> Set.Set comparable
toSet (TwoPSet a r) =
    Set.diff (CRDT.GSet.toSet a) (CRDT.GSet.toSet r)


encode : TwoPSet String -> Json.Encode.Value
encode (TwoPSet a r) =
    Json.Encode.list
        (Json.Encode.set Json.Encode.string)
        [ CRDT.GSet.toSet a
        , CRDT.GSet.toSet r
        ]


decoder : Json.Decode.Decoder (TwoPSet String)
decoder =
    Json.Decode.map2 TwoPSet
        (Json.Decode.index 0 CRDT.GSet.decoder)
        (Json.Decode.index 1 CRDT.GSet.decoder)
        |> Json.Decode.andThen
            (\(TwoPSet a r) ->
                if Set.intersect (CRDT.GSet.toSet a) (CRDT.GSet.toSet r) == (CRDT.GSet.toSet r) then
                    Json.Decode.succeed (TwoPSet a r)

                else
                    Json.Decode.fail "inconsistent TwoPSet state"
            )
