module CRDT.TwoPSet exposing
    ( TwoPSet
    , Op(..)
    , apply
    , decoder
    , empty
    , encode
    , fromList
    , insert
    , member
    , merge
    , remove
    , toList
    , toSet
    , patch
    )

import CRDT.GSet exposing (GSet)
import Json.Decode
import Json.Encode
import Set


type TwoPSet comparable
    = TwoPSet (GSet comparable) (GSet comparable)


type Op comparable
    = Insert comparable
    | Remove comparable


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


apply : Op comparable -> TwoPSet comparable -> TwoPSet comparable
apply op set =
    case op of
        Insert element ->
            insert element set

        Remove element ->
            remove element set


patch : List (Op comparable) -> TwoPSet comparable -> TwoPSet comparable
patch ops set =
    List.foldl apply set ops


fromList : List comparable -> TwoPSet comparable
fromList list =
    TwoPSet (CRDT.GSet.fromList list) CRDT.GSet.empty


toList : TwoPSet comparable -> List comparable
toList set =
    Set.toList (toSet set)


toSet : TwoPSet comparable -> Set.Set comparable
toSet (TwoPSet a r) =
    Set.diff (CRDT.GSet.toSet a) (CRDT.GSet.toSet r)


encode : (comparable -> Json.Encode.Value) -> TwoPSet comparable -> Json.Encode.Value
encode encoder (TwoPSet a r) =
    Json.Encode.list
        (CRDT.GSet.encode encoder)
        [ a, r ]


decoder : Json.Decode.Decoder comparable -> Json.Decode.Decoder (TwoPSet comparable)
decoder decode =
    Json.Decode.map2 TwoPSet
        (Json.Decode.index 0 (CRDT.GSet.decoder decode))
        (Json.Decode.index 1 (CRDT.GSet.decoder decode))
        |> Json.Decode.andThen
            (\(TwoPSet a r) ->
                if Set.intersect (CRDT.GSet.toSet a) (CRDT.GSet.toSet r) == CRDT.GSet.toSet r then
                    Json.Decode.succeed (TwoPSet a r)

                else
                    Json.Decode.fail "inconsistent TwoPSet state"
            )
