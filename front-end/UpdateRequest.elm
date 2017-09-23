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


module UpdateRequest
    exposing
        ( updateCommentSendHand
        )

import Alert
    exposing
        ( alertMessageTextInit
        , alertMessageTextServerAwaiting
        )
import Http
    exposing
        ( getString
        , send
        )
import MessageType
    exposing
        ( Msg
            ( CommentResponse
            )
        )
import ModelType
    exposing
        ( Model
        )
import Request
    exposing
        ( UriText
        , likeOrCommentRequestUriText
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        )
import UpdateLog
    exposing
        ( logMakeRequestAndFocus
        )
import UpdateStateVector
    exposing
        ( stateVector
        )


-- UPDATE


updateCommentSendHand : Model -> ( Model, Cmd Msg )
updateCommentSendHand model =
    let
        commentRequest : Cmd Msg
        commentRequest =
            send CommentResponse (getString commentRequestUriText)

        commentRequestUriText : UriText
        commentRequestUriText =
            likeOrCommentRequestUriText model.songCommenting model.userIdentifier model.commentText
    in
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            ( { model
                | alertMessageText = alertMessageTextServerAwaiting
              }
            , focusInputPossibly model
            )

        _ ->
            if String.isEmpty model.commentText then
                ( { model
                    | alertMessageText = alertMessageTextInit
                  }
                , focusInputPossibly model
                )
            else
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = True
                  }
                , logMakeRequestAndFocus model commentRequest "Request" commentRequestUriText
                )
