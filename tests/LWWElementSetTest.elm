module LWWElementSetTest exposing (suite)

import CRDT.LWWElementSet exposing (..)
import Expect exposing (Expectation)
import Json.Decode
import Json.Encode
import Test exposing (Test, describe, test)
import Time


now : Time.Posix
now =
    Time.millisToPosix 0


later : Time.Posix
later =
    Time.millisToPosix 1000


evenLater : Time.Posix
evenLater =
    Time.millisToPosix 2000


expectMember : String -> LWWElementSet String -> Expectation
expectMember a set =
    if member a set then
        Expect.pass

    else
        Expect.fail ("'" ++ a ++ "' not member of set")


expectNotMember : String -> LWWElementSet String -> Expectation
expectNotMember a set =
    if member a set then
        Expect.fail ("'" ++ a ++ "' is member of set")

    else
        Expect.pass


suite : Test
suite =
    describe "LWWElementSet"
        [ describe "merge"
            [ test "commutative" <|
                \_ ->
                    let
                        a =
                            remove "a" later (insert "a" now empty)

                        b =
                            remove "b" later (insert "b" now empty)
                    in
                    Expect.equal
                        (toSet (merge a b))
                        (toSet (merge b a))
            , test "associative" <|
                \_ ->
                    let
                        a =
                            remove "a" later (insert "a" now empty)

                        b =
                            remove "b" later (insert "b" now empty)

                        c =
                            remove "c" later (insert "c" now empty)
                    in
                    Expect.equal
                        (toSet (merge a (merge b c)))
                        (toSet (merge (merge a b) c))
            , test "idempotent" <|
                \_ ->
                    let
                        a =
                            remove "a" later (insert "a" now empty)
                    in
                    Expect.equal (toSet a) (toSet (merge a a))
            , test "union of sets" <|
                \_ ->
                    Expect.equal [ "a" ]
                        (toList
                            (merge
                                (insert "a" now empty)
                                (insert "a" later empty)
                            )
                        )
            , test "merging of concurrent updates" <|
                \_ ->
                    let
                        a =
                            remove "a" later (insert "a" now empty)

                        b =
                            remove "c" later (insert "b" now empty)

                        c =
                            insert "a" evenLater (insert "c" now empty)
                    in
                    Expect.all
                        [ expectMember "a"
                        , expectMember "b"
                        , expectNotMember "c"
                        , \set -> set |> toList |> Expect.equal [ "a", "b" ]
                        ]
                        (merge a (merge b c))
            ]
        , describe "apply"
            [ test "insert operation" <|
                  \_ -> Expect.equal [ "a" ] (toList (apply (Insert "a" now) empty))
            , test "remove operation" <|
                  \_ -> Expect.equal [ "a", "c" ]
                         (empty
                         |> apply (Insert "a" now)
                         |> apply (Insert "b" now)
                         |> apply (Remove "b" later)
                         |> apply (Insert "c" later)
                         |> toList
                         )
            ]
        , describe "patch"
            [ test "apply list of operations" <|
                  \_ -> Expect.equal [ "a", "c" ] (toList (patch
                                                               [ Insert "a" now
                                                               , Insert "b" now
                                                               , Remove "b" later
                                                               , Insert "c" later
                                                               ]
                                                               empty))
            ]
        , describe "encode"
            [ test "empty" <|
                \_ -> Expect.equal "[{},{}]" (Json.Encode.encode 0 (encode empty))
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal "[{\"a\":0,\"b\":0},{\"a\":1000}]"
                        (Json.Encode.encode 0
                            (encode (remove "a" later (insert "a" now (insert "b" now empty))))
                        )
            ]
        , describe "decode"
            [ test "empty" <|
                \_ ->
                    Expect.equal (Ok empty)
                        (Json.Decode.decodeString decoder "[{},{}]")
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal (Ok (remove "a" later (insert "a" now (insert "b" now empty))))
                        (Json.Decode.decodeString decoder "[{\"a\":0,\"b\":0},{\"a\":1000}]")
            ]
        ]
