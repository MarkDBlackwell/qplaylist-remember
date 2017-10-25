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
        , songsRecentResponseErr
        , songsRecentResponseOk
        )

import Alert
    exposing
        ( alertMessageTextErrorHttpLogging
        , alertMessageTextErrorHttpScreen
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
import DecodeSongsRecent
    exposing
        ( decodeSongsRecentResponse
        )
import ElmCycle
    exposing
        ( ElmCycle
        , Msg
            ( SongsRememberedStore
            )
        )
import Http
    exposing
        ( Error
        )
import ModelInitialize
    exposing
        ( awaitingServerResponseInit
        , commentTextInit
        )
import ModelType
    exposing
        ( Model
        )
import Song
    exposing
        ( likedOrCommentedShow
        )
import SongHelper
    exposing
        ( buttonIdReconstruct
        )
import SongInitialize
    exposing
        ( songCommentingMaybeInit
        , songLikingMaybeInit
        )
import SongType
    exposing
        ( SongsRemembered
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        , focusSetId
        )
import UpdateLog
    exposing
        ( logDecoding
        , logResponse
        )
import UpdateRequestType
    exposing
        ( ActionLikeOrComment
            ( Comment
            , Like
            )
        , HttpResponseText
        )
import Utilities
    exposing
        ( msg2Cmd
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
            |> logResponse
        , focusInputPossibly model
        ]
    )


commentResponseOk : Model -> HttpResponseText -> ActionLikeOrComment -> ElmCycle
commentResponseOk model httpResponseText actionLikeOrComment =
    let
        actionDescription : AlertMessageText
        actionDescription =
            case actionLikeOrComment of
                Comment ->
                    "send your Comment"

                Like ->
                    "send your Like"

        buttonCommand model =
            case actionLikeOrComment of
                Comment ->
                    Cmd.none

                Like ->
                    buttonCommandLike model

        buttonCommandAccomplished model =
            case actionLikeOrComment of
                Comment ->
                    buttonCommandComment model

                Like ->
                    buttonCommandLike model

        buttonCommandComment model =
            buttonIdReconstruct model.songsRemembered model.songCommentingMaybe "Comment"
                |> focusSetId

        buttonCommandLike model =
            buttonIdReconstruct model.songsRemembered model.songLikingMaybe "Like"
                |> focusSetId
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
                    |> logDecoding
                , buttonCommand model
                , focusInputPossibly model
                ]
            )

        Ok responseText ->
            if "ok" /= responseText then
                ( { model
                    | alertMessageText =
                        alertMessageTextSend actionDescription responseText
                            |> Just
                    , awaitingServerResponse = awaitingServerResponseInit
                    , songCommentingMaybe = songCommentingMaybeInit
                  }
                , Cmd.batch
                    [ Just responseText
                        |> logResponse
                    , buttonCommandAccomplished model
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
                    , songsRemembered = songsRememberedNew
                    , songCommentingMaybe = songCommentingMaybeInit
                    , commentText = commentTextInit
                  }
                , Cmd.batch
                    [ msg2Cmd SongsRememberedStore
                    , logResponse Nothing
                    , buttonCommand model
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
            |> logResponse
        , focusInputPossibly model
        ]
    )


likeResponseOk : Model -> HttpResponseText -> ElmCycle
likeResponseOk model httpResponseText =
    let
        actionDescription : AlertMessageText
        actionDescription =
            "send your Like"

        buttonCommand model =
            buttonCommandLike model

        buttonCommandAccomplished model =
            buttonCommand model

        buttonCommandLike model =
            buttonIdReconstruct model.songsRemembered model.songLikingMaybe "Like"
                |> focusSetId
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
                    |> logDecoding
                , buttonCommand model
                , focusInputPossibly model
                ]
            )

        Ok responseText ->
            if "ok" /= responseText then
                ( { model
                    | alertMessageText =
                        alertMessageTextSend actionDescription responseText
                            |> Just
                    , awaitingServerResponse = awaitingServerResponseInit
                    , songLikingMaybe = songLikingMaybeInit
                  }
                , Cmd.batch
                    [ Just responseText
                        |> logResponse
                    , buttonCommandAccomplished model
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
                    , songsRemembered = songsRememberedNew
                    , songLikingMaybe = songLikingMaybeInit
                  }
                , Cmd.batch
                    [ msg2Cmd SongsRememberedStore
                    , logResponse Nothing
                    , buttonCommand model
                    , focusInputPossibly model
                    ]
                )


songsRecentResponseErr : Model -> Error -> ElmCycle
songsRecentResponseErr model httpError =
    let
        alertMessageTextNew : AlertMessageText
        alertMessageTextNew =
            let
                actionDescription : String
                actionDescription =
                    "access the latest few songs"
            in
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
            |> logResponse
        , focusInputPossibly model
        ]
    )


songsRecentResponseOk : Model -> HttpResponseText -> ElmCycle
songsRecentResponseOk model httpResponseText =
    case decodeSongsRecentResponse httpResponseText of
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
            , Cmd.batch
                [ Just alertMessageTextDecode
                    |> logDecoding
                , focusInputPossibly model
                ]
            )

        Ok songsRecentNew ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , awaitingServerResponse = awaitingServerResponseInit
                , songsRecent = songsRecentNew
              }
              --Here, don't log the full response.
            , Cmd.batch
                [ logResponse Nothing
                , focusInputPossibly model
                ]
            )
