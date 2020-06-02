{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module LogUpdate exposing
    ( cmdLogDecoding
    , cmdLogRequest
    , cmdLogResponse
    )

import Debug
import ElmCycle
import RequestUpdateType
    exposing
        ( Action(..)
        )



-- UPDATE


cmdLogAction : Action -> Maybe String -> Cmd ElmCycle.Msg
cmdLogAction action textMaybe =
    let
        keepForConsoleLogging : String
        keepForConsoleLogging =
            let
                actionString : String
                actionString =
                    case action of
                        ActionDecoding ->
                            "Decoding"

                        ActionRequest ->
                            "Request"

                        ActionResponse ->
                            "Response"

                text : String
                text =
                    textMaybe
                        |> Maybe.withDefault "Ok"
            in
            text

        --Use Debug.log during development.
        --|> Debug.log actionString
    in
    Cmd.none


cmdLogDecoding : Maybe String -> Cmd ElmCycle.Msg
cmdLogDecoding textMaybe =
    textMaybe
        |> cmdLogAction ActionDecoding


cmdLogRequest : String -> Cmd ElmCycle.Msg
cmdLogRequest text =
    Just text
        |> cmdLogAction ActionRequest


cmdLogResponse : Maybe String -> Cmd ElmCycle.Msg
cmdLogResponse textMaybe =
    textMaybe
        |> cmdLogAction ActionResponse
