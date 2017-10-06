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
        ( ActionName
            ( Decoding
            , Response
            )
        , httpRequestOrResponseTextLog
        , logAndFocus
        , logMakeRequestAndFocus
        , logWithoutFocus
        )

import Alert
    exposing
        ( ActionName
        , AlertMessageText
        , AlertMessageTextMaybe
        )
import Debug
    exposing
        ( log
        )
import MessageType
    exposing
        ( Msg
            ( HttpRequestOrResponseTextLog
            )
        )
import ModelType
    exposing
        ( Model
        )
import Request
    exposing
        ( HttpRequestOrResponseText
        , RequestOrResponseLabelText
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        )
import UpdateUtilities
    exposing
        ( msg2Cmd
        )


-- UPDATE


type ActionName
    = Decoding
    | Response


actionName2String : ActionName -> String
actionName2String actionName =
    case actionName of
        Decoding ->
            "Decoding"

        Response ->
            "Response"


logAndFocus : Model -> ActionName -> AlertMessageTextMaybe -> Cmd Msg
logAndFocus model actionName alertMessageTextMaybe =
    Cmd.batch
        [ HttpRequestOrResponseTextLog
            (actionName2String actionName)
            (Maybe.withDefault "" alertMessageTextMaybe)
            |> msg2Cmd
        , focusInputPossibly model
        ]


logMakeRequestAndFocus : Model -> Cmd Msg -> AlertMessageText -> AlertMessageText -> Cmd Msg
logMakeRequestAndFocus model commandMessageRequest actionName alertMessageText =
    Cmd.batch
        [ HttpRequestOrResponseTextLog actionName alertMessageText
            |> msg2Cmd
        , commandMessageRequest
        , focusInputPossibly model
        ]


logWithoutFocus : Cmd Msg
logWithoutFocus =
    HttpRequestOrResponseTextLog
        (actionName2String Response)
        ""
        |> msg2Cmd


httpRequestOrResponseTextLog : Model -> RequestOrResponseLabelText -> HttpRequestOrResponseText -> ( Model, Cmd Msg )
httpRequestOrResponseTextLog model requestOrResponseLabelText httpRequestOrResponseText =
    let
        --Keep for console logging:
        a : String
        a =
            log requestOrResponseLabelText logText

        logText : String
        logText =
            if String.isEmpty httpRequestOrResponseText then
                "Ok"
            else
                httpRequestOrResponseText
    in
    ( model
    , focusInputPossibly model
    )
