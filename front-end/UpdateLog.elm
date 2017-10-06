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
        , logAndFocus
        , logMakeRequestAndFocus
        , logWithoutFocus
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
        ( Msg
            ( HttpRequestOrResponseTextLog
            )
        )
import ModelType
    exposing
        ( Model
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        )
import UpdateRequestHelper
    exposing
        ( actionName2String
        )
import UpdateRequestType
    exposing
        ( ActionName
            ( ActionRequest
            , ActionResponse
            )
        , HttpRequestOrResponseTextMaybe
        )
import UpdateUtilities
    exposing
        ( msg2Cmd
        )


-- UPDATE


httpRequestOrResponseTextLog : Model -> ActionName -> HttpRequestOrResponseTextMaybe -> ( Model, Cmd Msg )
httpRequestOrResponseTextLog model actionName httpRequestOrResponseTextMaybe =
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
    ( model
    , focusInputPossibly model
    )


logAndFocus : Model -> ActionName -> AlertMessageTextMaybe -> Cmd Msg
logAndFocus model actionName alertMessageTextMaybe =
    Cmd.batch
        [ HttpRequestOrResponseTextLog actionName alertMessageTextMaybe
            |> msg2Cmd
        , focusInputPossibly model
        ]


logMakeRequestAndFocus : Model -> Cmd Msg -> AlertMessageText -> Cmd Msg
logMakeRequestAndFocus model commandMessageRequest alertMessageText =
    Cmd.batch
        [ Just alertMessageText
            |> HttpRequestOrResponseTextLog ActionRequest
            |> msg2Cmd
        , commandMessageRequest
        , focusInputPossibly model
        ]


logWithoutFocus : Cmd Msg
logWithoutFocus =
    HttpRequestOrResponseTextLog ActionResponse Nothing
        |> msg2Cmd
