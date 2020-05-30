{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Subscription exposing (subscriptions)

import Browser.Events
import ElmCycle
    exposing
        ( Msg(..)
        )
import Json.Decode
import ModelType
    exposing
        ( Model
        )
import Platform.Sub
import ViewType
    exposing
        ( KeyChar
        )



-- SUBSCRIPTIONS
{- See:
   http://github.com/lenards/elm-example-key-decoding
   http://github.com/elm/browser/blob/53e3caa265fd9da3ec9880d47bb95eed6fe24ee6/notes/keyboard.md
   http://package.elm-lang.org/packages/elm/core/1.0.5/Platform-Sub
   http://package.elm-lang.org/packages/elm/browser/1.0.2/Browser-Events
   http://medium.com/jobteaser-dev-team/json-decoding-in-elm-explained-step-by-step-9d629b2625dc
-}


subscriptions : Model -> Platform.Sub.Sub ElmCycle.Msg
subscriptions model =
    let
        keyStrokeGlobal : Platform.Sub.Sub ElmCycle.Msg
        keyStrokeGlobal =
            let
                keySub : Platform.Sub.Sub KeyChar
                keySub =
                    let
                        decoder : Json.Decode.Decoder KeyChar
                        decoder =
                            Json.Decode.string
                                |> Json.Decode.field "key"

                        source : Json.Decode.Decoder msg -> Platform.Sub.Sub msg
                        source =
                            Browser.Events.onKeyUp
                    in
                    source decoder
            in
            keySub
                |> Platform.Sub.map MsgKeystrokeHand
    in
    Platform.Sub.batch
        [ keyStrokeGlobal
        ]
