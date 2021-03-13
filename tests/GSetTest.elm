module GSetTest exposing (suite)

import CRDT.GSet exposing (..)
import Expect
import Json.Decode
import Json.Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "GSet"
        [ describe "merge"
            [ test "commutative" <|
                \_ ->
                    let
                        a =
                            insert "a" empty

                        b =
                            insert "b" empty
                    in
                    Expect.equal (merge a b) (merge b a)
            , test "associative" <|
                \_ ->
                    let
                        a =
                            insert "a" empty

                        b =
                            insert "b" empty

                        c =
                            insert "c" empty
                    in
                    Expect.equal (merge a (merge b c)) (merge (merge a b) c)
            , test "idempotent" <|
                \_ ->
                    Expect.equal empty (merge empty empty)
            , test "union of sets" <|
                \_ ->
                    Expect.equal [ "a" ] (toList (merge (insert "a" empty) (insert "a" empty)))
            , test "merging of concurrent updates" <|
                \_ ->
                    let
                        a =
                            insert "a" empty

                        b =
                            insert "b" empty

                        c =
                            insert "a" (insert "c" empty)
                    in
                    Expect.equal [ "a", "b", "c" ] (toList (merge a (merge b c)))
            ]
        , describe "encode"
            [ test "empty" <|
                \_ -> Expect.equal "[]" (Json.Encode.encode 0 (encode empty))
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal "[\"a\",\"b\"]"
                        (Json.Encode.encode 0
                            (encode (insert "a" (insert "b" empty)))
                        )
            ]
        , describe "decode"
            [ test "empty" <|
                \_ ->
                    Expect.equal (Ok empty)
                        (Json.Decode.decodeString decoder "[]")
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal (Ok (insert "a" (insert "b" empty)))
                        (Json.Decode.decodeString decoder "[\"a\",\"b\"]")
            ]
        ]
