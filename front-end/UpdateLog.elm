{- Copyright (C) 2017 Mark D. Blackwell.
   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.
   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
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
import MessageType
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
                action2String : Action -> String
                action2String action =
                    case action of
                        ActionDecoding ->
                            "Decoding"

                        ActionRequest ->
                            "Request"

                        ActionResponse ->
                            "Response"

                logText : String
                logText =
                    Maybe.withDefault "Ok" textMaybe
            in
            action2String action
                |> flip log logText
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
