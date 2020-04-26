{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UpdateResponse exposing
    ( likeOrCommentResponseErr
    , likeOrCommentResponseOk
    , songsRecentResponseErr
    , songsRecentResponseOk
    )

import Alert
    exposing
        ( actionDescriptionRecent
        , alertMessageTextErrorHttpLogging
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
        , Msg(..)
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
        ( ActionLikeOrComment(..)
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
        modelNewSongLikingOrCommenting : Model
        modelNewSongLikingOrCommenting =
            case actionLikeOrComment of
                Comment ->
                    model

                Like ->
                    { model
                        | songLikingMaybe = songLikingMaybeInit
                    }
    in
    ( { modelNewSongLikingOrCommenting
        | alertMessageText =
            actionLikeOrComment2String actionLikeOrComment
                |> alertMessageTextRequestLikeOrComment httpError
                |> Just
        , awaitingServerResponse = awaitingServerResponseInit
      }
    , Cmd.batch
        [ (++)
            (alertMessageTextErrorHttpLogging httpError)
            (actionLikeOrComment2String actionLikeOrComment
                |> alertMessageTextRequestLikeOrComment httpError
            )
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
            actionLikeOrComment2String actionLikeOrComment
                |> String.append "send your "

        buttonCommand : Cmd Msg
        buttonCommand =
            case actionLikeOrComment of
                Comment ->
                    Cmd.none

                Like ->
                    buttonCommandAccomplished

        buttonCommandAccomplished : Cmd Msg
        buttonCommandAccomplished =
            actionLikeOrComment2String actionLikeOrComment
                |> buttonIdReconstruct
                    model.songsRemembered
                    songLikingOrCommentingMaybe
                |> focusSetId

        modelNewCommentText : Model
        modelNewCommentText =
            case actionLikeOrComment of
                Comment ->
                    { modelNewSongLikingOrCommenting
                        | commentText = commentTextInit
                    }

                Like ->
                    modelNewSongLikingOrCommenting

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

        songLikingOrCommentingMaybe : SongRememberedMaybe
        songLikingOrCommentingMaybe =
            case actionLikeOrComment of
                Comment ->
                    model.songCommentingMaybe

                Like ->
                    model.songLikingMaybe
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
                [ (++)
                    alertMessageTextDecode
                    httpResponseText
                    |> Just
                    |> logDecoding
                , buttonCommand
                , focusInputPossibly model
                ]
            )

        Ok responseText ->
            let
                serverSaysRequestWasBad : Bool
                serverSaysRequestWasBad =
                    "good" /= responseText
            in
            if serverSaysRequestWasBad then
                ( { modelNewSongLikingOrCommenting
                    | alertMessageText =
                        alertMessageTextSend actionDescription responseText
                            |> Just
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , Cmd.batch
                    [ (++)
                        responseText
                        httpResponseText
                        |> Just
                        |> logResponse
                    , buttonCommand
                    , focusInputPossibly model
                    ]
                )

            else
                let
                    songsRememberedNew : SongsRemembered
                    songsRememberedNew =
                        likedOrCommentedShow
                            songLikingOrCommentingMaybe
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
                    , buttonCommandAccomplished
                    , focusInputPossibly model
                    ]
                )


songsRecentResponseErr : Model -> Error -> ElmCycle
songsRecentResponseErr model httpError =
    let
        alertMessageTextNew : AlertMessageText
        alertMessageTextNew =
            String.concat
                [ alertMessageTextErrorHttpScreen httpError
                , " (while attempting to "
                , actionDescriptionRecent
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
            ( { model
                | alertMessageText =
                    alertMessageTextSend actionDescriptionRecent alertMessageTextDecode
                        |> Just
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Cmd.batch
                [ (++)
                    alertMessageTextDecode
                    httpResponseText
                    |> Just
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
