module LSeqTest exposing (..)

import CRDT.LSeq exposing (..)
import Expect
import Random
import Test exposing (..)


seed : Random.Seed
seed =
    Random.initialSeed 0


emptySeq : LSeq Char
emptySeq =
    empty seed


abcSeq : LSeq Char
abcSeq =
    fromString seed "abc"


find : Char -> LSeq Char -> Id
find c seq =
    toList seq
        |> List.foldl
            (\( el, id ) acc ->
                if el == c then
                    id

                else
                    acc
            )
            []


suite : Test
suite =
    describe "LSeq"
        [ describe "length"
            [ test "is zero for empty sequence" <|
                \_ -> Expect.equal 0 (length emptySeq)
            , test "returns length of sequence" <|
                \_ -> Expect.equal 3 (length abcSeq)
            , test "excluding removed elements" <|
                \_ -> Expect.equal 2 (length (dropRight 1 abcSeq))
            ]
        , describe "toList"
            [ test "elements as list" <|
                \_ -> Expect.equal [ 'a', 'b', 'c' ] (toList abcSeq |> List.map Tuple.first)
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
                    Expect.equal emptySeq (dropRight 1 abcSeq)
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
        ]
