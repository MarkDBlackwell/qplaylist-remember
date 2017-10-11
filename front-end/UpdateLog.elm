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
        ( httpRequestOrResponseTextLog
        , logDecoding
        , logRequest
        , logResponse
        )

import AlertType
    exposing
        ( AlertMessageText
        , AlertMessageTextMaybe
        )
import Debug
    exposing
        ( log
        )
import MessageType
    exposing
        ( ElmCycle
        , Msg
        )
import UpdateRequestHelper
    exposing
        ( actionName2String
        )
import UpdateRequestType
    exposing
        ( ActionName
            ( ActionDecoding
            , ActionRequest
            , ActionResponse
            )
        , HttpRequestOrResponseTextMaybe
        )


-- UPDATE


httpRequestOrResponseTextLog : ActionName -> HttpRequestOrResponseTextMaybe -> Cmd Msg
httpRequestOrResponseTextLog actionName httpRequestOrResponseTextMaybe =
    let
        --Keep for console logging:
        a : String
        a =
            log
                (actionName2String actionName)
                logText

        logText : String
        logText =
            Maybe.withDefault "Ok" httpRequestOrResponseTextMaybe
    in
    Cmd.none


logAction : ActionName -> AlertMessageTextMaybe -> Cmd Msg
logAction actionName alertMessageTextMaybe =
    httpRequestOrResponseTextLog actionName alertMessageTextMaybe


logDecoding : AlertMessageTextMaybe -> Cmd Msg
logDecoding alertMessageTextMaybe =
    logAction ActionDecoding alertMessageTextMaybe


logRequest : AlertMessageText -> Cmd Msg
logRequest alertMessageText =
    Just alertMessageText
        |> logAction ActionRequest


logResponse : AlertMessageTextMaybe -> Cmd Msg
logResponse alertMessageTextMaybe =
    logAction ActionResponse alertMessageTextMaybe
