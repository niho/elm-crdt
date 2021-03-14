module ORSetTest exposing (..)

import CRDT.ORSet exposing (..)
import Expect exposing (Expectation)
import Json.Decode
import Json.Encode
import Test exposing (Test, describe, test)
import Time


expectMember : String -> ORSet String -> Expectation
expectMember a set =
    if member a set then
        Expect.pass

    else
        Expect.fail ("'" ++ a ++ "' not member of set")


expectNotMember : String -> ORSet String -> Expectation
expectNotMember a set =
    if member a set then
        Expect.fail ("'" ++ a ++ "' is member of set")

    else
        Expect.pass


suite : Test
suite =
    describe "ORSet"
        [ describe "merge"
            [ test "commutative" <|
                \_ ->
                    let
                        a =
                            remove "a" (insert "a" "1" empty)

                        b =
                            remove "b" (insert "b" "1" empty)
                    in
                    Expect.equal
                        (toSet (merge a b))
                        (toSet (merge b a))
            , test "associative" <|
                \_ ->
                    let
                        a =
                            remove "a" (insert "a" "1" empty)

                        b =
                            remove "b" (insert "b" "1" empty)

                        c =
                            remove "c" (insert "c" "1" empty)
                    in
                    Expect.equal
                        (toSet (merge a (merge b c)))
                        (toSet (merge (merge a b) c))
            , test "idempotent" <|
                \_ ->
                    let
                        a =
                            remove "a" (insert "a" "1" empty)
                    in
                    Expect.equal (toSet a) (toSet (merge a a))
            , test "merging of replicas" <|
                \_ ->
                    Expect.equal [ "a" ]
                        (toList
                            (merge
                                (insert "a" "1" empty)
                                (insert "a" "2" empty)
                            )
                        )
            , test "merging of concurrent updates" <|
                \_ ->
                    let
                        a =
                            insert "b" "1" (insert "a" "1" empty)

                        b =
                            remove "c" (insert "c" "2" (insert "b" "2" empty))

                        c =
                            insert "a" "3" (insert "b" "3" (insert "c" "3" empty))
                    in
                    Expect.all
                        [ expectMember "a"
                        , expectMember "b"
                        , expectNotMember "c"
                        , \set -> set |> toList |> Expect.equal [ "a", "b" ]
                        ]
                        (merge a (merge b c))
            ]
        , describe "encode"
            [ test "empty" <|
                \_ -> Expect.equal "[{},{}]" (Json.Encode.encode 0 (encode empty))
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal "[{\"a\":[\"1\"],\"b\":[\"1\",\"2\"]},{\"a\":[\"1\"]}]"
                        (Json.Encode.encode 0
                            (encode
                                (remove "a"
                                    (insert "a"
                                        "1"
                                        (insert "b"
                                            "2"
                                            (insert "b" "1" empty)
                                        )
                                    )
                                )
                            )
                        )
            ]
        , describe "decode"
            [ test "empty" <|
                \_ ->
                    Expect.equal (Ok empty)
                        (Json.Decode.decodeString decoder "[{},{}]")
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal
                        (Ok
                            (remove "a"
                                (insert "a"
                                    "1"
                                    (insert "b"
                                        "2"
                                        (insert "b" "1" empty)
                                    )
                                )
                            )
                        )
                        (Json.Decode.decodeString decoder
                            "[{\"a\":[\"1\"],\"b\":[\"1\",\"2\"]},{\"a\":[\"1\"]}]"
                        )
            ]
        ]
