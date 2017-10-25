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
        ( likeOrCommentResponseErr
        , likeOrCommentResponseOk
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
        ( SongRememberedMaybe
        , SongsRemembered
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        , focusSetId
        )
import UpdateHelper
    exposing
        ( actionLikeOrComment2String
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


likeOrCommentResponseErr : Model -> Error -> ActionLikeOrComment -> ElmCycle
likeOrCommentResponseErr model httpError actionLikeOrComment =
    let
        actionLikeOrCommentText : String
        actionLikeOrCommentText =
            actionLikeOrComment2String actionLikeOrComment

        modelNew : Model
        modelNew =
            case actionLikeOrComment of
                Comment ->
                    model

                Like ->
                    { model
                        | songLikingMaybe = songLikingMaybeInit
                    }
    in
    ( { modelNew
        | alertMessageText =
            alertMessageTextRequestLikeOrComment httpError actionLikeOrCommentText
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


likeOrCommentResponseOk : Model -> HttpResponseText -> ActionLikeOrComment -> ElmCycle
likeOrCommentResponseOk model httpResponseText actionLikeOrComment =
    let
        actionDescription : AlertMessageText
        actionDescription =
            String.append
                "send your "
                actionLikeOrCommentText

        actionLikeOrCommentText : String
        actionLikeOrCommentText =
            actionLikeOrComment2String actionLikeOrComment

        buttonCommand : Cmd Msg
        buttonCommand =
            case actionLikeOrComment of
                Comment ->
                    Cmd.none

                Like ->
                    buttonCommandAccomplished

        buttonCommandAccomplished : Cmd Msg
        buttonCommandAccomplished =
            buttonIdReconstruct
                model.songsRemembered
                modelLikingOrCommenting
                actionLikeOrCommentText
                |> focusSetId

        modelLikingOrCommenting : SongRememberedMaybe
        modelLikingOrCommenting =
            case actionLikeOrComment of
                Comment ->
                    model.songCommentingMaybe

                Like ->
                    model.songLikingMaybe

        modelNewSongLikingOrCommenting : Model
        modelNewSongLikingOrCommenting =
            case actionLikeOrComment of
                Comment ->
                    { model
                        | songCommentingMaybe = songCommentingMaybeInit
                    }

                Like ->
                    { model
                        | songLikingMaybe = songLikingMaybeInit
                    }

        modelNewCommentText : Model
        modelNewCommentText =
            case actionLikeOrComment of
                Comment ->
                    { modelNewSongLikingOrCommenting
                        | commentText = commentTextInit
                    }

                Like ->
                    modelNewSongLikingOrCommenting
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
                , buttonCommand
                , focusInputPossibly model
                ]
            )

        Ok responseText ->
            if "ok" /= responseText then
                ( { modelNewSongLikingOrCommenting
                    | alertMessageText =
                        alertMessageTextSend actionDescription responseText
                            |> Just
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , Cmd.batch
                    [ Just responseText
                        |> logResponse
                    , buttonCommandAccomplished
                    , focusInputPossibly model
                    ]
                )
            else
                let
                    songsRememberedNew : SongsRemembered
                    songsRememberedNew =
                        likedOrCommentedShow
                            modelLikingOrCommenting
                            model.songsRemembered
                in
                ( { modelNewCommentText
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = awaitingServerResponseInit
                    , songsRemembered = songsRememberedNew
                  }
                , Cmd.batch
                    [ msg2Cmd SongsRememberedStore
                    , logResponse Nothing
                    , buttonCommand
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
