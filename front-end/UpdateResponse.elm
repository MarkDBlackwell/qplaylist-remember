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
        ( commentResponseErr
        , commentResponseOk
        , likeResponseErr
        , likeResponseOk
        , songsLatestResponseErr
        , songsLatestResponseOk
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
            ( SongsRememberedStore
            )
        )
import ModelType
    exposing
        ( Model
        )
import Request
    exposing
        ( ActionName
            ( ActionDecoding
            , Response
            )
        , HttpResponseText
        )
import Song
    exposing
        ( SongsRemembered
        , likedOrCommentedShow
        , songCommentingMaybeInit
        , songLikingMaybeInit
        )
import UpdateLog
    exposing
        ( logAndFocus
        , logWithoutFocus
        )
import UpdateUtilities
    exposing
        ( msg2Cmd
        )


-- UPDATE


commentResponseErr : Model -> Error -> ( Model, Cmd Msg )
commentResponseErr model httpError =
    ( { model
        | alertMessageText =
            alertMessageTextRequestLikeOrComment httpError "comment"
                |> Just
        , awaitingServerResponse = awaitingServerResponseInit
      }
    , alertMessageTextErrorHttpLogging httpError
        |> Just
        |> logAndFocus model Response
    )


commentResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
commentResponseOk model httpResponseText =
    let
        actionDescription : AlertMessageText
        actionDescription =
            "send your Comment"
    in
    case decodeLikeOrCommentResponse httpResponseText of
        Err alertMessageTextDecode ->
            ( { model
                | alertMessageText =
                    alertMessageTextSend actionDescription alertMessageTextDecode
                        |> Just
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Just alertMessageTextDecode
                |> logAndFocus model ActionDecoding
            )

        Ok responseString ->
            if "ok" /= responseString then
                ( { model
                    | alertMessageText =
                        alertMessageTextSend actionDescription responseString
                            |> Just
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , Just responseString
                    |> logAndFocus model Response
                )
            else
                let
                    songsRememberedNew : SongsRemembered
                    songsRememberedNew =
                        likedOrCommentedShow
                            model.songCommentingMaybe
                            model.songsRemembered
                in
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = awaitingServerResponseInit
                    , commentText = commentTextInit
                    , songCommentingMaybe = songCommentingMaybeInit
                    , songsRemembered = songsRememberedNew
                  }
                , Cmd.batch
                    [ msg2Cmd SongsRememberedStore
                    , logWithoutFocus
                    ]
                )


likeResponseErr : Model -> Error -> ( Model, Cmd Msg )
likeResponseErr model httpError =
    ( { model
        | alertMessageText =
            alertMessageTextRequestLikeOrComment httpError "Like"
                |> Just
        , awaitingServerResponse = awaitingServerResponseInit
        , songLikingMaybe = songLikingMaybeInit
      }
    , alertMessageTextErrorHttpLogging httpError
        |> Just
        |> logAndFocus model Response
    )


likeResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
likeResponseOk model httpResponseText =
    let
        actionDescription : AlertMessageText
        actionDescription =
            "send your Like"
    in
    case decodeLikeOrCommentResponse httpResponseText of
        Err alertMessageTextDecode ->
            ( { model
                | alertMessageText =
                    alertMessageTextSend actionDescription alertMessageTextDecode
                        |> Just
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Just alertMessageTextDecode
                |> logAndFocus model ActionDecoding
            )

        Ok responseString ->
            if "ok" /= responseString then
                ( { model
                    | alertMessageText =
                        alertMessageTextSend actionDescription responseString
                            |> Just
                    , awaitingServerResponse = awaitingServerResponseInit
                    , songLikingMaybe = songLikingMaybeInit
                  }
                , Just responseString
                    |> logAndFocus model Response
                )
            else
                let
                    songsRememberedNew : SongsRemembered
                    songsRememberedNew =
                        likedOrCommentedShow
                            model.songLikingMaybe
                            model.songsRemembered
                in
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = awaitingServerResponseInit
                    , songLikingMaybe = songLikingMaybeInit
                    , songsRemembered = songsRememberedNew
                  }
                , Cmd.batch
                    [ msg2Cmd SongsRememberedStore
                    , logAndFocus model Response Nothing
                    ]
                )


songsLatestResponseErr : Model -> Error -> ( Model, Cmd Msg )
songsLatestResponseErr model httpError =
    let
        actionDescription : String
        actionDescription =
            "access the latest few songs"

        alertMessageTextNew : AlertMessageText
        alertMessageTextNew =
            String.concat
                [ alertMessageTextErrorHttpScreen httpError
                , " (while attempting to "
                , actionDescription
                , ")"
                ]
    in
    ( { model
        | alertMessageText = Just alertMessageTextNew
        , awaitingServerResponse = awaitingServerResponseInit
      }
    , alertMessageTextErrorHttpLogging httpError
        |> Just
        |> logAndFocus model Response
    )


songsLatestResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
songsLatestResponseOk model httpResponseText =
    case decodeSongsLatest httpResponseText of
        Err alertMessageTextDecode ->
            let
                actionDescription : AlertMessageText
                actionDescription =
                    "access the latest few songs"
            in
            ( { model
                | alertMessageText =
                    alertMessageTextSend actionDescription alertMessageTextDecode
                        |> Just
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Just alertMessageTextDecode
                |> logAndFocus model ActionDecoding
            )

        Ok songsLatestNew ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , awaitingServerResponse = awaitingServerResponseInit
                , songsLatest = songsLatestNew
              }
              --Here, don't log the full response.
            , logAndFocus model Response Nothing
            )
