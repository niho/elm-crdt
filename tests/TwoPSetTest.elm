module TwoPSetTest exposing (suite)

import CRDT.TwoPSet exposing (..)
import Expect
import Json.Decode
import Json.Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "TwoPSet"
        [ describe "merge"
            [ test "commutative" <|
                \_ ->
                    let
                        a =
                            remove "a" (insert "a" empty)

                        b =
                            remove "b" (insert "b" empty)
                    in
                    Expect.equal (merge a b) (merge b a)
            , test "associative" <|
                \_ ->
                    let
                        a =
                            remove "a" (insert "a" empty)

                        b =
                            remove "b" (insert "b" empty)

                        c =
                            remove "c" (insert "c" empty)
                    in
                    Expect.equal (merge a (merge b c)) (merge (merge a b) c)
            , test "idempotent" <|
                \_ ->
                    let
                        a =
                            remove "a" (insert "a" empty)
                    in
                    Expect.equal a (merge a a)
            , test "union of sets" <|
                \_ ->
                    Expect.equal [ "a" ] (toList (merge (insert "a" empty) (insert "a" empty)))
            , test "merging of concurrent updates" <|
                \_ ->
                    let
                        a =
                            remove "a" (insert "a" empty)

                        b =
                            insert "b" empty

                        c =
                            insert "a" (insert "c" empty)
                    in
                    Expect.equal [ "b", "c" ] (toList (merge a (merge b c)))
            ]
        , describe "apply"
            [  test "insert operation" <|
                  \_ -> Expect.equal [ 1 ] (toList (apply (Insert 1) empty))
            , test "remove operation" <|
                  \_ -> Expect.equal [ 1, 3 ]
                         (empty
                         |> apply (Insert 1)
                         |> apply (Insert 2)
                         |> apply (Remove 2)
                         |> apply (Insert 3)
                         |> toList
                         )
            ]
        , describe "patch"
            [ test "apply list of operations" <|
                  \_ -> Expect.equal [ 1, 3 ] (toList (patch
                                                           [ Insert 1
                                                           , Insert 2
                                                           , Remove 2
                                                           , Insert 3
                                                           ]
                                                           empty))
            ]
        , describe "encode"
            [ test "empty" <|
                \_ -> Expect.equal "[[],[]]" (Json.Encode.encode 0 (encode empty))
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal "[[\"a\",\"b\"],[\"a\"]]"
                        (Json.Encode.encode 0
                            (encode (remove "a" (insert "a" (insert "b" empty))))
                        )
            ]
        , describe "decode"
            [ test "empty" <|
                \_ ->
                    Expect.equal (Ok empty)
                        (Json.Decode.decodeString decoder "[[],[]]")
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal (Ok (remove "a" (insert "a" (insert "b" empty))))
                        (Json.Decode.decodeString decoder "[[\"a\",\"b\"],[\"a\"]]")
            , test "fail on inconsistent data" <|
                \_ ->
                    Expect.equal Nothing
                        (Json.Decode.decodeString decoder "[[\"b\"],[\"a\"]]"
                            |> Result.toMaybe
                        )
            ]
        ]
