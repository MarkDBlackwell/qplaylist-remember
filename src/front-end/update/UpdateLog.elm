{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UpdateLog exposing
    ( logDecoding
    , logRequest
    , logResponse
    )

import Debug
import ElmCycle
import UpdateRequestType
    exposing
        ( Action(..)
        )



-- UPDATE


logAction : Action -> Maybe String -> Cmd ElmCycle.Msg
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
                    textMaybe
                        |> Maybe.withDefault "Ok"
            in
            text

        --Use Debug.log during development.
        --|> Debug.log actionString
    in
    Cmd.none


logDecoding : Maybe String -> Cmd ElmCycle.Msg
logDecoding textMaybe =
    textMaybe
        |> logAction ActionDecoding


logRequest : String -> Cmd ElmCycle.Msg
logRequest text =
    Just text
        |> logAction ActionRequest


logResponse : Maybe String -> Cmd ElmCycle.Msg
logResponse textMaybe =
    textMaybe
        |> logAction ActionResponse
