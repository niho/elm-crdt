module RGATest exposing (suite)

import CRDT.RGA exposing (..)
import Expect
import Json.Decode
import Json.Encode
import Test exposing (Test, describe, test)


suite : Test
suite =
    describe "RGA"
        [ describe "empty"
              [ test "return sequence with no elements" <|
                    \_ ->
                        Expect.equal 0 (List.length (toList (empty "test")))
              ]
        , describe "merge"
              [ test "concurrent edits" <|
                    \_ ->
                        let
                            str = fromString "a" "CMD"
                            
                            a =
                                str |> insert "a" -1 'T'
                                    |> insert "a" -1 'R'
                                    |> insert "a" -1 'L'
                                    |> remove "a" 2
                                    |> remove "a" 3

                            b =
                                str |> insert "b" -1 'D'
                                    |> insert "b" -1 'E'
                                    |> insert "b" -1 'L'

                            c =
                                str |> insert "c" -1 'A'
                                    |> insert "c" -1 'L'
                                    |> insert "c" -1 'T'
                        in
                        Expect.equal "CTRLALTDEL"
                            (toString (merge (merge a b) c))

              ]
        , describe "fromString"
              [ test "identity" <|
                    \_ ->
                        Expect.equal "Hello" (toString (fromString "test" "Hello"))
              ]
        , describe "encode"
              [ test "empty" <|
                    \_ ->
                        Expect.equal "[]" (Json.Encode.encode 0 (encode (empty "test")))
              , test "sequence" <|
                    \_ ->
                        Expect.equal ""
                            (Json.Encode.encode 0 (encode (fromString "test" "Hello")))
              ]
        ]
