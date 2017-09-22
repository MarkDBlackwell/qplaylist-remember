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


module UpdateResponseDetails
    exposing
        ( logAndFocus
        , logMakeRequestAndFocus
        , logWithoutFocus
        )

import Alert
    exposing
        ( AlertMessageText
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
import UpdateUtilities
    exposing
        ( msg2Cmd
        )


-- UPDATE


logAndFocus : Model -> AlertMessageText -> AlertMessageText -> Cmd Msg
logAndFocus model actionName alertMessageText =
    Cmd.batch
        [ msg2Cmd (HttpRequestOrResponseTextLog actionName alertMessageText)
        , focusInputPossibly model
        ]


logMakeRequestAndFocus : Model -> Cmd Msg -> AlertMessageText -> AlertMessageText -> Cmd Msg
logMakeRequestAndFocus model commandMessageRequest actionName alertMessageText =
    Cmd.batch
        [ msg2Cmd (HttpRequestOrResponseTextLog actionName alertMessageText)
        , commandMessageRequest
        , focusInputPossibly model
        ]


logWithoutFocus : AlertMessageText -> Cmd Msg
logWithoutFocus actionName =
    msg2Cmd (HttpRequestOrResponseTextLog actionName "")
