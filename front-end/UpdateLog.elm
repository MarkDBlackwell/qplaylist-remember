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
        , logAndFocusId
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
import Dom
    exposing
        ( Id
        )
import MessageType
    exposing
        ( ElmCycle
        , Msg
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
        , focusSetId
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
import Utilities
    exposing
        ( msg2Cmd
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


logAndFocus : Model -> ActionName -> AlertMessageTextMaybe -> Cmd Msg
logAndFocus model actionName alertMessageTextMaybe =
    Cmd.batch
        [ HttpRequestOrResponseTextLog actionName alertMessageTextMaybe
            |> msg2Cmd
        , focusInputPossibly model
        ]


logAndFocusId : Model -> Id -> Cmd Msg
logAndFocusId model id =
    Cmd.batch
        [ HttpRequestOrResponseTextLog ActionResponse Nothing
            |> msg2Cmd
        , focusSetId id
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
    --TODO: Maybe this is no longer used.
    HttpRequestOrResponseTextLog ActionResponse Nothing
        |> msg2Cmd
