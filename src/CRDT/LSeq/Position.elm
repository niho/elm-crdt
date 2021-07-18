module CRDT.LSeq.Position exposing
    ( Id
    , Position
    , add
    , baseAt
    , compareId
    , comparePos
    , diff
    , fromPos
    , maxId
    , minId
    , prefix
    , sub
    )

import Bitwise
import Random


type alias Level =
    Int


type Id
    = Id Position


type alias Position =
    List Int


minId : Id
minId =
    --Id 0 1
    Id [ 0 ]


maxId : Id
maxId =
    --Id 0xFFFFFFFF 1
    Id [ baseAt 1 ]


rootBits : Int
rootBits =
    4


maxDigits : Int
maxDigits =
    5


baseAt : Level -> Int
baseAt depth =
    (2 ^ rootBits) - 1


fromPos : List Int -> Id
fromPos pos =
    Id pos



{- case pos of
   [] ->
       Id 0 0

   p :: rest ->
       case fromPos rest of
           Id i l ->
               Id (i + p) (l + 1)
-}
{- prefix : Id -> Int -> Int
   prefix (Id id l) depth_ =
       {- let

          in
              if depth_ == 0 then
                  0
              else
                  if id < (base depth_) then
                      (prefix id (depth_ - 1)) +
                          (Bitwise.and ((base (depth_)) - 1) id)

                  else
                      (prefix id (depth_ - 1)) + (base depth_)
       -}
       if depth_ <= 1 then
           id
               |> Bitwise.and ((2 ^ rootBits) - 1)
               |> Bitwise.shiftLeftBy 26

       else
           prefix (Id id l) (depth_ - 1)
               |> Bitwise.shiftLeftBy ((round (logBase 2 (toFloat rootBits)) + 1) + (depth_ - 1))
               |> Bitwise.or id
-}


prefix : Id -> Level -> Id
prefix (Id id) depth =
    if List.length id >= depth then
        List.take depth id |> Id

    else
        id ++ List.repeat ((depth - List.length id)) 0 |> Id


comparePos : Position -> Position -> Order
comparePos a b =
    compareId (fromPos a) (fromPos b)


compareId : Id -> Id -> Order
compareId (Id a) (Id b) =
    case ( a, b ) of
        ( [], [] ) ->
            EQ

        ( [], h :: rest ) ->
            LT

        ( h :: rest, [] ) ->
            GT

        ( h1 :: rest1, h2 :: rest2 ) ->
            case compare h1 h2 of
                EQ ->
                    compareId (Id rest1) (Id rest2)

                LT ->
                    LT

                GT ->
                    GT



--compare (prefix (Id a l1) (max l1 l2)) (prefix (Id b l2) (max l1 l2))


add : ( Id, Level ) -> Int -> Id
add ( id, depth ) i =
    --Id (prefix (Id id l) depth + i) depth
    case prefix id depth of
        Id pos ->
            case List.reverse pos of
                [] ->
                    Id []

                x :: xs ->
                    ([ x + i ] ++ xs) |> List.reverse |> Id


sub : ( Id, Level ) -> Int -> Id
sub ( id, depth ) i =
    --Id (prefix (Id id l) depth - i) depth
    case prefix id depth of
        Id pos ->
            case List.reverse pos of
                [] ->
                    Id []

                x :: xs ->
                    ([ x - i ] ++ xs) |> List.reverse |> Id


diff : Id -> Id -> Level -> Int
diff a b depth =
    let
        (Id a_) =
            prefix a depth

        (Id b_) =
            prefix b depth
    in
    List.map2 (-) b_ a_
        |> List.sum
