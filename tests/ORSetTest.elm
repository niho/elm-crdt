module ORSetTest exposing (..)

import CRDT.ORSet exposing (..)
import Expect exposing (Expectation)
import Json.Decode
import Json.Encode
import Test exposing (Test, describe, test)
import Time


expectMember : comparable -> ORSet comparable -> Expectation
expectMember a set =
    if member a set then
        Expect.pass

    else
        Expect.fail ("'" ++ (Debug.toString a) ++ "' not member of set")


expectNotMember : comparable -> ORSet comparable -> Expectation
expectNotMember a set =
    if member a set then
        Expect.fail ("'" ++ (Debug.toString a) ++ "' is member of set")

    else
        Expect.pass


suite : Test
suite =
    describe "ORSet"
        [ describe "member"
              [ test "added is member" <|
                    \_ ->
                        Expect.equal True (member 1 (insert 1 "a" empty))
              , test "never added is not member" <|
                    \_ ->
                        Expect.equal False (member 2 (insert 1 "a" empty))
              , test "removed is not member" <|
                    \_ ->
                        Expect.equal False
                            (empty
                            |> insert 1 "a"
                            |> insert 1 "b"
                            |> remove 1
                            |> member 1
                            )
              ]
        , describe "merge"
            [ test "commutative" <|
                \_ ->
                    let
                        a =
                            remove 1 (insert 2 "a" (insert 1 "a" empty))

                        b =
                            remove 1 (insert 1 "b" (insert 2 "b" empty))
                    in
                    Expect.equal
                        (toSet (merge a b))
                        (toSet (merge b a))
            , test "associative" <|
                \_ ->
                    let
                        a =
                            insert 1 "a" empty

                        b =
                            remove 2 (insert 2 "b" empty)

                        c =
                            remove 3 (insert 4 "c" (insert 3 "c" empty))
                    in
                    Expect.equal
                        (toSet (merge a (merge b c)))
                        (toSet (merge (merge a b) c))
            , test "idempotent" <|
                \_ ->
                    let
                        a =
                            remove 1 (insert 2 "a" (insert 1 "a" empty))
                    in
                    Expect.equal (toSet a) (toSet (merge a a))
            , test "add wins" <|
                \_ ->
                    Expect.equal [ 1, 2, 3 ]
                        (toList
                            (merge
                                (insert 3 "a" (remove 2 (insert 2 "a" (insert 1 "a" empty))))
                                (insert 2 "b" (insert 1 "b" empty))
                            )
                        )
            , test "remove" <|
                \_ ->
                    Expect.equal [ 2 ]
                        (toList
                            (merge
                                (remove 1 (insert 1 "b" (insert 1 "a" empty)))
                                (insert 2 "b" (insert 1 "b" empty))
                            )
                        )
            , test "remove converges" <|
                \_ ->
                    Expect.equal [ 2 ]
                        (toList
                            (merge
                                (remove 1 (insert 1 "a" empty))
                                (remove 1 (insert 2 "b" (insert 1 "b" empty)))
                            )
                        )
            , test "local remove" <|
                \_ ->
                    Expect.equal [ 2 ]
                        (toList
                            (merge
                                (remove 1 (insert 2 "b" (insert 1 "a" empty)))
                                (insert 2 "b" empty)
                            )
                        )
            ]
        , describe "apply"
            [ test "insert operation" <|
                  \_ -> Expect.equal [ 1 ] (toList (apply (Insert 1 "a") empty))
            , test "remove operation" <|
                  \_ -> Expect.equal [ 1, 3 ]
                         (empty
                         |> apply (Insert 1 "a")
                         |> apply (Insert 2 "b")
                         |> apply (Remove 2)
                         |> apply (Insert 3 "c")
                         |> toList
                         )
            ]
        , describe "patch"
            [ test "apply list of operations" <|
                  \_ -> Expect.equal [ 1, 3 ] (toList (patch
                                                           [ Insert 1 "a"
                                                           , Insert 2 "b"
                                                           , Remove 2
                                                           , Insert 3 "c"
                                                           ]
                                                           empty))
            ]
        , describe "encode"
            [ test "empty" <|
                \_ -> Expect.equal "[{},{}]" (Json.Encode.encode 0 (encode empty))
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal "[{\"1\":[\"a\"],\"2\":[\"a\",\"b\"]},{\"1\":[\"a\"]}]"
                        (Json.Encode.encode 0
                            (encode
                                (remove "1"
                                    (insert "1"
                                        "a"
                                        (insert "2"
                                            "b"
                                            (insert "2" "a" empty)
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
                            (remove "1"
                                (insert "1"
                                    "a"
                                    (insert "2"
                                        "b"
                                        (insert "2" "a" empty)
                                    )
                                )
                            )
                        )
                        (Json.Decode.decodeString decoder
                            "[{\"1\":[\"a\"],\"2\":[\"a\",\"b\"]},{\"1\":[\"a\"]}]"
                        )
            ]
        ]
