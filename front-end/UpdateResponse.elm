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


module UpdateResponse
    exposing
        ( updateCommentResponseErr
        )

import Alert
    exposing
        ( alertMessageTextErrorHttpLogging
        , alertMessageTextRequestLikeOrComment
        )
import Http
    exposing
        ( Error
        )
import Initialize
    exposing
        ( awaitingServerResponseInit
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


updateCommentResponseErr : Model -> Error -> ( Model, Cmd Msg )
updateCommentResponseErr model httpError =
    ( { model
        | alertMessageText = alertMessageTextRequestLikeOrComment httpError "comment"
        , awaitingServerResponse = awaitingServerResponseInit
      }
    , Cmd.batch
        [ msg2Cmd (HttpRequestOrResponseTextLog "Response" (alertMessageTextErrorHttpLogging httpError))
        , focusInputPossibly model
        ]
    )
