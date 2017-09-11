module Example exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)


suite : Test
suite =
    --Implement our first test.
    --See http://package.elm-lang.org/packages/elm-community/elm-test/latest for how to do this!
    describe "some tests"
        [ describe "some sub tests"
            [ test "has no effect on a palindrome" <|
                \_ ->
                    let
                        palindrome =
                            "hannah"
                    in
                        Expect.equal palindrome (String.reverse palindrome)
            ]
        ]
