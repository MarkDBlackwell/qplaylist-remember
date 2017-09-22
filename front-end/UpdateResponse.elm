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
        , updateCommentResponseOk
        , updateLikeResponseErr
        , updateLikeResponseOk
        , updateSongsLatestResponseErr
        , updateSongsLatestResponseOk
        )

import Alert
    exposing
        ( AlertMessageText
        , alertMessageTextErrorHttpLogging
        , alertMessageTextErrorHttpScreen
        , alertMessageTextErrorUnexpected
        , alertMessageTextInit
        , alertMessageTextRequestLikeOrComment
        , alertMessageTextSend
        )
import DecodeLikeOrCommentResponse
    exposing
        ( decodeLikeOrCommentResponse
        )
import DecodeSongsLatest
    exposing
        ( decodeSongsLatest
        )
import Http
    exposing
        ( Error
        )
import Initialize
    exposing
        ( awaitingServerResponseInit
        , commentTextInit
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
        ( HttpResponseText
        )
import Song
    exposing
        ( SongsRemembered
        , likedOrCommentedShow
        , songCommentingInit
        , songLikingInit
        )
import UpdateResponseDetails
    exposing
        ( logAndFocus
        , logMakeRequestAndFocus
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
    , logAndFocus model "Response" (alertMessageTextErrorHttpLogging httpError)
    )


updateCommentResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
updateCommentResponseOk model httpResponseText =
    let
        actionDescription : AlertMessageText
        actionDescription =
            "send your Comment"
    in
    case decodeLikeOrCommentResponse httpResponseText of
        Err alertMessageTextDecode ->
            ( { model
                | alertMessageText = alertMessageTextSend actionDescription alertMessageTextDecode
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , logAndFocus model "Decoding" alertMessageTextDecode
            )

        Ok responseString ->
            if "ok" /= responseString then
                ( { model
                    | alertMessageText = alertMessageTextSend actionDescription responseString
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , logAndFocus model "Response" responseString
                )
            else
                let
                    songsRememberedNew : SongsRemembered
                    songsRememberedNew =
                        likedOrCommentedShow model.songCommenting model.songsRemembered
                in
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = awaitingServerResponseInit
                    , commentText = commentTextInit
                    , songCommenting = songCommentingInit
                    , songsRemembered = songsRememberedNew
                  }
                  --Here, don't focus.
                , msg2Cmd (HttpRequestOrResponseTextLog "Response" "")
                )


updateLikeResponseErr : Model -> Error -> ( Model, Cmd Msg )
updateLikeResponseErr model httpError =
    ( { model
        | alertMessageText = alertMessageTextRequestLikeOrComment httpError "Like"
        , awaitingServerResponse = awaitingServerResponseInit
        , songLiking = songLikingInit
      }
    , logAndFocus model "Response" (alertMessageTextErrorHttpLogging httpError)
    )


updateLikeResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
updateLikeResponseOk model httpResponseText =
    let
        actionDescription : AlertMessageText
        actionDescription =
            "send your Like"
    in
    case decodeLikeOrCommentResponse httpResponseText of
        Err alertMessageTextDecode ->
            ( { model
                | alertMessageText = alertMessageTextSend actionDescription alertMessageTextDecode
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , logAndFocus model "Decoding" alertMessageTextDecode
            )

        Ok responseString ->
            if "ok" /= responseString then
                ( { model
                    | alertMessageText = alertMessageTextSend actionDescription responseString
                    , awaitingServerResponse = awaitingServerResponseInit
                    , songLiking = songLikingInit
                  }
                , logAndFocus model "Response" responseString
                )
            else
                let
                    songsRememberedNew : SongsRemembered
                    songsRememberedNew =
                        likedOrCommentedShow model.songLiking model.songsRemembered
                in
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = awaitingServerResponseInit
                    , songLiking = songLikingInit
                    , songsRemembered = songsRememberedNew
                  }
                , logAndFocus model "Response" ""
                )


updateSongsLatestResponseErr : Model -> Error -> ( Model, Cmd Msg )
updateSongsLatestResponseErr model httpError =
    let
        actionDescription : AlertMessageText
        actionDescription =
            "access the latest few songs"

        alertMessageTextNew : AlertMessageText
        alertMessageTextNew =
            alertMessageTextErrorHttpScreen httpError
                ++ " (while attempting to "
                ++ actionDescription
                ++ ")"
    in
    ( { model
        | alertMessageText = alertMessageTextNew
        , awaitingServerResponse = awaitingServerResponseInit
      }
    , logAndFocus model "Response" (alertMessageTextErrorHttpLogging httpError)
    )


updateSongsLatestResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
updateSongsLatestResponseOk model httpResponseText =
    case decodeSongsLatest httpResponseText of
        Err alertMessageTextDecode ->
            let
                actionDescription : AlertMessageText
                actionDescription =
                    "access the latest few songs"
            in
            ( { model
                | alertMessageText = alertMessageTextSend actionDescription alertMessageTextDecode
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , logAndFocus model "Decoding" alertMessageTextDecode
            )

        Ok songsLatestNew ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , awaitingServerResponse = awaitingServerResponseInit
                , songsLatest = songsLatestNew
              }
              --Here, don't log the full response.
            , logAndFocus model "Response" ""
            )
