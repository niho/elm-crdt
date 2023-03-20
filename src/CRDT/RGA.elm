module CRDT.RGA exposing
    ( RGA
    , Operation
    , empty
    , insert
    , remove
    , merge
    , apply
    , patch
    , toList
    , toString
    , fromString
    , encode
    , decoder
    )

import Json.Decode
import Json.Encode


type alias Replica =
    String


type alias VPtr =
    ( Int, Replica )

        
type Op a
    = Insert VPtr a
    | Remove VPtr


type alias Operation a =
    ( VPtr, Op a )


type RGA a
    = RGA (List (Operation a)) Int


empty : Replica -> RGA a
empty id =
    RGA [] 0


head : Replica -> VPtr
head id =
    ( 0, id )


ptr : Int -> List (Operation a) -> VPtr
ptr pos seq =
    head


insert : Replica -> Int -> a -> RGA a -> RGA a
insert id after value (RGA seq idx) =
    RGA (( ( idx + 1, id ), (Insert (ptr after seq) value) ) :: seq) (idx + 1)


remove : Replica -> Int -> RGA a -> RGA a
remove id pos (RGA seq idx) =
    RGA (( ( idx + 1, id ), (Remove (ptr pos seq)) ) :: seq) idx


sortSeq : List (Operation a) -> List (Operation a)
sortSeq seq =
    let
        compareOp (a,_) (b,_) =
            compareVPtr a b

        compareVPtr (a,aRep) (b,bRep) =
            case compare a b of
                LT -> LT
                GT -> GT
                EQ -> compare aRep bRep
    in
        List.sortWith compareOp seq
        

merge : RGA a -> RGA a -> RGA a
merge (RGA aSeq aIdx) (RGA bSeq bIdx) =
    RGA (sortSeq (List.append aSeq bSeq)) (max aIdx bIdx)


apply : Operation a -> RGA a -> RGA a
apply ((opIdx, replica), op) (RGA seq idx) =
    RGA (sortSeq (((opIdx,replica), op) :: seq)) (max opIdx idx)


patch : List (Operation a) -> RGA a -> RGA a
patch ops rga =
    List.foldl apply rga ops


toList : RGA a -> List a
toList (RGA seq _) =
    let
        reducer (_,op) acc =
            case op of
                Insert _ value ->
                    value :: acc

                Remove _ ->
                    List.drop 1 acc
    in
    List.foldr reducer [] seq


toString : RGA Char -> String
toString rga =
    String.fromList (toList rga)


fromString : Replica -> String -> RGA Char
fromString replica str =
    String.foldl (insert replica (head replica)) (empty replica) str


encode : RGA Char -> Json.Encode.Value
encode (RGA seq _) =
    let
        encodeItem ( ptr, op ) =
            Json.Encode.list identity
                [ encodeVPtr ptr, encodeOp op ]

        encodeVPtr ( idx, replica ) =
            Json.Encode.list identity
                [ Json.Encode.int idx
                , Json.Encode.string replica
                ]

        encodeOp op =
            case op of
                Insert after value ->
                    Json.Encode.list identity
                        [ Json.Encode.int 0
                        , encodeVPtr after
                        , Json.Encode.string (String.fromChar value)
                        ]

                Remove ptr ->
                    Json.Encode.list identity
                        [ Json.Encode.int 1
                        , encodeVPtr ptr
                        ]
    in
    Json.Encode.list encodeItem seq


decoder : Json.Decode.Decoder (RGA a)
decoder =
    Json.Decode.fail "not implemented yet"
