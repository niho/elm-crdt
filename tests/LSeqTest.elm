module LSeqTest exposing (..)

import Array
import CRDT.LSeq exposing (..)
import CRDT.LSeq.Position exposing (..)
import Expect
import Random
import Test exposing (..)


seed : Random.Seed
seed =
    Random.initialSeed 0


emptySeq : LSeq Char
emptySeq =
    empty seed



--abcSeq : LSeq Char
--abcSeq =
--    fromString seed "abc"


find : Char -> LSeq Char -> Maybe Id
find c seq =
    toList seq
        |> List.foldl
            (\( el, id ) acc ->
                if el == c then
                    Just id

                else
                    acc
            )
            Nothing


suite : Test
suite =
    describe "LSeq"
        [ {- describe "length"
                 [ test "is zero for empty sequence" <|
                     \_ -> Expect.equal 0 (length emptySeq)
                 , test "returns length of sequence" <|
                     \_ -> Expect.equal 3 (length abcSeq)
                 , test "excluding removed elements" <|
                     \_ -> Expect.equal 2 (length (dropRight 1 abcSeq))
                 ]
             , describe "toList"
                 [ test "elements as list" <|
                     \_ ->
                         Expect.equal [ 'a', 'b', 'c' ]
                             (toList abcSeq |> List.map Tuple.first)
                 , test "identifiers as list" <|
                     \_ ->
                         Expect.equal [ 26, 28, 60 ]
                             (toList abcSeq |> List.map Tuple.second)
                 , test "as list" <|
                     \_ ->
                         Expect.equal
                             [ ( 'a', 26 )
                             , ( 'b', 28 )
                             , ( 'c', 60 )
                             ]
                             (toList abcSeq)
                 ]
             , describe "fromList"
                 [ test "sequence from list" <|
                     \_ -> Expect.equal abcSeq (fromList seed [ 'a', 'b', 'c' ])
                 ]
             , describe "toString"
                 [ test "char elements as string" <|
                     \_ -> Expect.equal "abc" (toString abcSeq)
                 ]
             , describe "fromString"
                 [ test "char sequence from string" <|
                     \_ -> Expect.equal abcSeq (fromString seed "abc")
                 ]
             , describe "append"
                 [ test "adds element to end" <|
                     \_ -> Expect.equal "abcd" (toString (append 'd' abcSeq))
                 ]
             , describe "insert"
                 [ todo "inserts between p and q"
                 ]
             , describe "dropRight"
                 [ test "removes n elements from end of sequence" <|
                     \_ ->
                         Expect.equal "ab" (toString (dropRight 1 abcSeq))
                 ]
             , describe "remove"
                 [ test "removes element at id" <|
                     \_ ->
                         let
                             seq =
                                 abcSeq

                             id =
                                 find 'b' seq
                         in
                         Expect.equal "ac" (toString (remove id seq))
                 ]
             , describe "merge"
                 [ test "converges" <|
                     \_ ->
                         let
                             seq =
                                 fromString seed "abcd"

                             p =
                                 find 'b' seq

                             q =
                                 find 'c' seq
                         in
                         Expect.equal "abefcd"
                             (toString
                                 (merge
                                     (insert 'e' p q seq)
                                     (insert 'f' p q seq)
                                 )
                             )
                 , todo "preserves partial order"
                 ]
          -}
          describe "alloc" <|
            [ test "alloc long" <|
                \_ ->
                    Expect.equal emptySeq (fromString seed "abcdef")
            ]
        , describe "Position"
            [ describe "add"
                [ test "increment" <|
                    \_ -> Expect.equal (fromPos [ 1 ]) (add ( minId, 1 ) 1)
                , test "increment at depth" <|
                    \_ -> Expect.equal (fromPos [ 0, 1 ]) (add ( minId, 2 ) 1)
                ]
            , describe "sub"
                [ test "decrement" <|
                    \_ -> Expect.equal (fromPos [ 14 ]) (sub ( maxId, 1 ) 1)
                , test "decrement at depth" <|
                    \_ -> Expect.equal (fromPos [ 0, 30 ]) (sub ( maxId, 2 ) 1)
                ]
            , describe "diff"
                [ test "min/max" <|
                    \_ -> Expect.equal (baseAt 1) (diff minId maxId 1)
                ]
            , describe "prefix"
                [ test "minId" <|
                    \_ -> Expect.equal (fromPos [ 0, 0 ]) (prefix minId 2)
                , test "maxId" <|
                    \_ -> Expect.equal (fromPos [ 15, 0 ]) (prefix maxId 2)
                , test "padding" <|
                    \_ ->
                        Expect.equal (fromPos [ 2, 5, 0, 0 ])
                            (prefix (fromPos [ 2, 5 ]) 4)
                , test "trim" <|
                    \_ ->
                        Expect.equal (fromPos [ 2 ]) (prefix (fromPos [ 2, 5 ]) 1)
                ]
            , describe "comparePos"
                [ test "equal" <|
                    \_ ->
                        Expect.all
                            [ Expect.equal (comparePos [] [])
                            , Expect.equal (comparePos [ 1 ] [ 1 ])
                            , Expect.equal (comparePos [ 1, 2 ] [ 1, 2 ])
                            , Expect.equal (comparePos [ 1, 2, 3, 4, 5 ] [ 1, 2, 3, 4, 5 ])
                            ]
                            EQ
                , test "not equal" <|
                    \_ ->
                        Expect.all
                            [ Expect.notEqual (comparePos [] [ 1 ])
                            , Expect.notEqual (comparePos [ 1 ] [ 2 ])
                            , Expect.notEqual (comparePos [ 1 ] [ 1, 2 ])
                            , Expect.notEqual (comparePos [ 1, 2 ] [ 1 ])
                            ]
                            EQ
                , test "less than" <|
                    \_ ->
                        Expect.all
                            [ Expect.equal (comparePos [ 0 ] [ 1 ])
                            , Expect.equal (comparePos [ 0 ] [ 99 ])
                            , Expect.equal (comparePos [ 1 ] [ 1, 2 ])
                            , Expect.equal (comparePos [ 2, 1 ] [ 3 ])
                            , Expect.equal (comparePos [ 2, 3, 4, 5 ] [ 2, 3, 4, 5, 0 ])
                            , Expect.equal (comparePos [ 42, 6 ] [ 98, 7, 38, 5, 9, 68 ])
                            , Expect.equal (comparePos [ 0, 53 ] [ 99, 12 ])
                            ]
                            LT
                , test "greater than" <|
                    \_ ->
                        Expect.all
                            [ Expect.equal (comparePos [ 1 ] [ 0 ])
                            , Expect.equal (comparePos [ 99 ] [ 0 ])
                            , Expect.equal (comparePos [ 1, 2 ] [ 1 ])
                            , Expect.equal (comparePos [ 2 ] [ 1, 9 ])
                            , Expect.equal (comparePos [ 2, 3, 4, 5, 0 ] [ 2, 3, 4, 5 ])
                            , Expect.equal (comparePos [ 98, 7, 38, 5, 9, 68 ] [ 42, 6 ])
                            , Expect.equal (comparePos [ 99, 12 ] [ 0, 53, 26 ])
                            ]
                            GT
                ]
            ]

        {- , describe "applyInsert"
           [ test "x" <|
               \_ ->
                   Expect.equal (Array.fromList [ [ 0 ], [ 2 ], [ 26 ], [ 99 ] ])
                       (applyInsert [ 26 ] (Array.fromList [ [ 0 ], [ 2 ], [ 99 ] ]))
           ]
        -}
        ]
