{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UpdateLog
    exposing
        ( logDecoding
        , logRequest
        , logResponse
        )

import Debug
    exposing
        ( log
        )
import ElmCycle
    exposing
        ( Msg
        )
import UpdateRequestType
    exposing
        ( Action
            ( ActionDecoding
            , ActionRequest
            , ActionResponse
            )
        )


-- UPDATE


logAction : Action -> Maybe String -> Cmd Msg
logAction action textMaybe =
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
                    Maybe.withDefault "Ok" textMaybe
            in
            log actionString text
    in
    Cmd.none


logDecoding : Maybe String -> Cmd Msg
logDecoding textMaybe =
    logAction ActionDecoding textMaybe


logRequest : String -> Cmd Msg
logRequest text =
    Just text
        |> logAction ActionRequest


logResponse : Maybe String -> Cmd Msg
logResponse textMaybe =
    logAction ActionResponse textMaybe
