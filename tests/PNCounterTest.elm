module PNCounterTest exposing (suite)

import CRDT.PNCounter exposing (..)
import Expect
import Json.Decode
import Json.Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "PNCounter"
        [ describe "zero"
            [ test "return counter with value set to zero" <|
                \_ ->
                    Expect.equal 0 (value zero)
            ]
        , describe "increment"
            [ test "increments the counter" <|
                \_ ->
                    Expect.equal 1 (value (increment "test" zero))
            ]
        , describe "decrement"
            [ test "decrements the counter" <|
                \_ ->
                    Expect.equal -1 (value (decrement "test" zero))
            ]
        , describe "merge"
            [ test "commutative" <|
                \_ ->
                    let
                        a =
                            zero

                        b =
                            increment "test" zero
                    in
                    Expect.equal (merge a b) (merge b a)
            , test "associative" <|
                \_ ->
                    let
                        a =
                            zero

                        b =
                            increment "test" zero

                        c =
                            increment "test" (increment "test" zero)
                    in
                    Expect.equal (merge a (merge b c)) (merge (merge a b) c)
            , test "idempotent" <|
                \_ ->
                    Expect.equal zero (merge zero zero)
            , test "selects max value on conflict" <|
                \_ ->
                    Expect.equal 1 (value (merge zero (increment "test" zero)))
            , test "merging of concurrent updates" <|
                \_ ->
                    let
                        a =
                            increment "a" (increment "a" zero)

                        b =
                            increment "a" (increment "b" zero)

                        c =
                            zero
                    in
                    Expect.equal 3 (value (merge a (merge b c)))
            ]
        , describe "encode"
            [ test "zero" <|
                \_ -> Expect.equal "[{},{}]" (Json.Encode.encode 0 (encode zero))
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal "[{\"a\":1,\"b\":1},{\"c\":1}]"
                        (Json.Encode.encode 0
                            (encode (decrement "c" (increment "a" (increment "b" zero))))
                        )
            ]
        , describe "decode"
            [ test "zero" <|
                \_ ->
                    Expect.equal (Ok zero)
                        (Json.Decode.decodeString decoder "[{},{}]")
            , test "concurrenct updates" <|
                \_ ->
                    Expect.equal "[{\"a\":1,\"b\":1},{\"c\":1}]"
                        (Json.Encode.encode 0
                            (encode (decrement "c" (increment "a" (increment "b" zero))))
                        )
            ]
        ]
