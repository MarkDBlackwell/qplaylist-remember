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
        ( ElmCycle
        , Msg
            ( HttpRequestOrResponseTextLog
            , SongsRememberedStore
            )
        )
import ModelType
    exposing
        ( Model
        )
import Song
    exposing
        ( buttonIdReconstruct
        , likedOrCommentedShow
        , song2SongTimeless
        , songCommentingMaybeInit
        , songLikingMaybeInit
        , songs2SongsTimeless
        )
import SongType
    exposing
        ( SongTimeless
        , SongsRemembered
        , SongsRememberedIndex
        , SongsTimeless
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        , focusSetId
        )
import UpdateLog
    exposing
        ( logAction
        , logAndFocusOld
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


commentResponseErr : Model -> Error -> ElmCycle
commentResponseErr model httpError =
    ( { model
        | alertMessageText =
            alertMessageTextRequestLikeOrComment httpError "comment"
                |> Just
        , awaitingServerResponse = awaitingServerResponseInit
      }
    , Cmd.batch
        [ alertMessageTextErrorHttpLogging httpError
            |> Just
            |> logAction ActionResponse
        , focusInputPossibly model
        ]
    )


commentResponseOk : Model -> HttpResponseText -> ElmCycle
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
            , Cmd.batch
                [ Just alertMessageTextDecode
                    |> logAction ActionDecoding
                , focusInputPossibly model
                ]
            )

        Ok responseString ->
            if "ok" /= responseString then
                ( { model
                    | alertMessageText =
                        alertMessageTextSend actionDescription responseString
                            |> Just
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , Cmd.batch
                    [ Just responseString
                        |> logAction ActionResponse
                    , focusInputPossibly model
                    ]
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
                    , HttpRequestOrResponseTextLog ActionResponse Nothing
                        |> msg2Cmd
                    , buttonIdReconstruct model.songsRemembered model.songCommentingMaybe "Comment"
                        |> focusSetId
                    , focusInputPossibly model
                    ]
                )


likeResponseErr : Model -> Error -> ElmCycle
likeResponseErr model httpError =
    ( { model
        | alertMessageText =
            alertMessageTextRequestLikeOrComment httpError "Like"
                |> Just
        , awaitingServerResponse = awaitingServerResponseInit
        , songLikingMaybe = songLikingMaybeInit
      }
    , Cmd.batch
        [ alertMessageTextErrorHttpLogging httpError
            |> Just
            |> logAction ActionResponse
        , focusInputPossibly model
        ]
    )


likeResponseOk : Model -> HttpResponseText -> ElmCycle
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
            , Cmd.batch
                [ Just alertMessageTextDecode
                    |> logAction ActionDecoding
                , focusInputPossibly model
                ]
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
                , Cmd.batch
                    [ Just responseString
                        |> logAction ActionResponse
                    , focusInputPossibly model
                    ]
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
                    , logAction ActionResponse Nothing
                    , focusInputPossibly model
                    ]
                )


songsLatestResponseErr : Model -> Error -> ElmCycle
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
    , Cmd.batch
        [ alertMessageTextErrorHttpLogging httpError
            |> Just
            |> logAction ActionResponse
        , focusInputPossibly model
        ]
    )


songsLatestResponseOk : Model -> HttpResponseText -> ElmCycle
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
                |> logAndFocusOld model ActionDecoding
            )

        Ok songsLatestNew ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , awaitingServerResponse = awaitingServerResponseInit
                , songsLatest = songsLatestNew
              }
              --Here, don't log the full response.
            , logAndFocusOld model ActionResponse Nothing
            )
