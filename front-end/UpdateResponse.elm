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
        ( alertMessageTextErrorHttpLogging
        , alertMessageTextErrorHttpScreen
        , alertMessageTextErrorUnexpected
        , alertMessageTextInit
        , alertMessageTextRequestLikeOrComment
        , alertMessageTextSend
        )
import AlertType
    exposing
        ( AlertMessageText
        )
import DecodeLikeOrCommentResponse
    exposing
        ( decodeLikeOrCommentResponse
        )
import DecodeSongsLatest
    exposing
        ( decodeSongsLatestResponse
        )
import Dom
    exposing
        ( Id
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
import Song
    exposing
        ( likedOrCommentedShow
        , song2SongTimeless
        , songCommentingMaybeInit
        , songLikingMaybeInit
        , songs2SongsTimeless
        )
import SongType
    exposing
        ( SongCommenting
        , SongTimeless
        , SongsRemembered
        , SongsRememberedIndex
        , SongsTimeless
        )
import UpdateLog
    exposing
        ( logAndFocus
        , logAndFocusId
        )
import UpdateRequestType
    exposing
        ( ActionName
            ( ActionDecoding
            , ActionResponse
            )
        , HttpResponseText
        , LikeOrCommentResponseText
        )
import Utilities
    exposing
        ( matchingIndexes
        , msg2Cmd
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
        |> logAndFocus model ActionResponse
    )


commentingIndexMaybe : Model -> SongCommenting -> Maybe SongsRememberedIndex
commentingIndexMaybe model songCommenting =
    let
        songsRememberedTimeless : SongsTimeless
        songsRememberedTimeless =
            songs2SongsTimeless model.songsRemembered
    in
    song2SongTimeless songCommenting
        |> matchingIndexes songsRememberedTimeless
        |> List.head


commentResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
commentResponseOk model httpResponseText =
    let
        actionDescription : AlertMessageText
        actionDescription =
            "send your Comment"

        idToFocusOn : Id
        idToFocusOn =
            let
                createButtonId : SongsRememberedIndex -> Id
                createButtonId songsRememberedIndex =
                    String.concat
                        [ "buttonComment"
                        , toString songsRememberedIndex
                        ]

                handleSongCommenting : SongCommenting -> Id
                handleSongCommenting songCommenting =
                    commentingIndexMaybe model songCommenting
                        |> Maybe.map (\x -> createButtonId x)
                        |> Maybe.withDefault "refresh"
            in
            model.songCommentingMaybe
                |> Maybe.map (\x -> handleSongCommenting x)
                |> Maybe.withDefault "refresh"
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
                    |> logAndFocus model ActionResponse
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
                    , logAndFocusId model idToFocusOn
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
        |> logAndFocus model ActionResponse
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
                    |> logAndFocus model ActionResponse
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
                    , logAndFocus model ActionResponse Nothing
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
        |> logAndFocus model ActionResponse
    )


songsLatestResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
songsLatestResponseOk model httpResponseText =
    case decodeSongsLatestResponse httpResponseText of
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
            , logAndFocus model ActionResponse Nothing
            )
