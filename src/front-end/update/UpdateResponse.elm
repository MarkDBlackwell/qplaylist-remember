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
import AlertType
    exposing
        ( AlertMessageText
        )
import DecodeLikeOrCommentResponse
    exposing
        ( decodeLikeOrCommentResponse
        )
import DecodeSongsRecent
import ElmCycle
    exposing
        ( ElmCycle
        , Msg(..)
        )
import Http
import ModelInitialize
import ModelType
    exposing
        ( Model
        )
import Song
import SongHelper
import SongInitialize
import SongType
    exposing
        ( SongRememberedMaybe
        , SongsRemembered
        )
import UpdateFocus
import UpdateHelper
import UpdateLog
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


alertLogging : Http.Error -> AlertMessageText
alertLogging httpError =
    httpError
        |> Alert.alertMessageTextErrorHttpLogging


likeOrCommentResponseErr : Model -> Http.Error -> ActionLikeOrComment -> ElmCycle
likeOrCommentResponseErr model httpError actionLikeOrComment =
    let
        alertLikeOrComment : AlertType.LikeOrCommentName -> AlertMessageText
        alertLikeOrComment =
            httpError
                |> Alert.alertMessageTextRequestLikeOrComment

        modelNewSongLikingOrCommenting : Model
        modelNewSongLikingOrCommenting =
            case actionLikeOrComment of
                Comment ->
                    model

                Like ->
                    { model
                        | songLikingMaybe = SongInitialize.songLikingMaybeInit
                    }
    in
    ( { modelNewSongLikingOrCommenting
        | alertMessageText =
            actionLikeOrComment
                |> UpdateHelper.actionLikeOrComment2String
                |> alertLikeOrComment
                |> Just
        , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
      }
    , Cmd.batch
        [ (++)
            (httpError
                |> alertLogging
            )
            (actionLikeOrComment
                |> UpdateHelper.actionLikeOrComment2String
                |> alertLikeOrComment
            )
            |> Just
            |> UpdateLog.logResponse
        , UpdateFocus.focusInputPossibly model
        ]
    )


likeOrCommentResponseOk : Model -> HttpResponseText -> ActionLikeOrComment -> ElmCycle
likeOrCommentResponseOk model httpResponseText actionLikeOrComment =
    let
        actionDescription : AlertMessageText
        actionDescription =
            actionLikeOrComment
                |> UpdateHelper.actionLikeOrComment2String
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
            actionLikeOrComment
                |> UpdateHelper.actionLikeOrComment2String
                |> SongHelper.buttonIdReconstruct
                    model.songsRemembered
                    songLikingOrCommentingMaybe
                |> UpdateFocus.focusSetId

        modelNewCommentText : Model
        modelNewCommentText =
            case actionLikeOrComment of
                Comment ->
                    { modelNewSongLikingOrCommenting
                        | commentText = ModelInitialize.commentTextInit
                    }

                Like ->
                    modelNewSongLikingOrCommenting

        modelNewSongLikingOrCommenting : Model
        modelNewSongLikingOrCommenting =
            case actionLikeOrComment of
                Comment ->
                    { model
                        | songCommentingMaybe = SongInitialize.songCommentingMaybeInit
                    }

                Like ->
                    { model
                        | songLikingMaybe = SongInitialize.songLikingMaybeInit
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
                    alertMessageTextDecode
                        |> Alert.alertMessageTextSend actionDescription
                        |> Just
                , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
              }
            , Cmd.batch
                [ (++)
                    alertMessageTextDecode
                    httpResponseText
                    |> Just
                    |> UpdateLog.logDecoding
                , buttonCommand
                , UpdateFocus.focusInputPossibly model
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
                        responseText
                            |> Alert.alertMessageTextSend actionDescription
                            |> Just
                    , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
                  }
                , Cmd.batch
                    [ (++)
                        responseText
                        httpResponseText
                        |> Just
                        |> UpdateLog.logResponse
                    , buttonCommand
                    , UpdateFocus.focusInputPossibly model
                    ]
                )

            else
                let
                    songsRememberedNew : SongsRemembered
                    songsRememberedNew =
                        model.songsRemembered
                            |> Song.likedOrCommentedShow
                                songLikingOrCommentingMaybe
                in
                ( { modelNewCommentText
                    | alertMessageText = Alert.alertMessageTextInit
                    , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
                    , songsRemembered = songsRememberedNew
                  }
                , Cmd.batch
                    [ msg2Cmd SongsRememberedStore
                    , UpdateLog.logResponse Nothing
                    , buttonCommandAccomplished
                    , UpdateFocus.focusInputPossibly model
                    ]
                )


songsRecentResponseErr : Model -> Http.Error -> ElmCycle
songsRecentResponseErr model httpError =
    let
        alertMessageTextNew : AlertMessageText
        alertMessageTextNew =
            let
                alertScreen : String
                alertScreen =
                    httpError
                        |> Alert.alertMessageTextErrorHttpScreen
            in
            String.concat
                [ alertScreen
                , " (while attempting to "
                , Alert.actionDescriptionRecent
                , ")"
                ]
    in
    ( { model
        | alertMessageText = Just alertMessageTextNew
        , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
      }
    , Cmd.batch
        [ httpError
            |> alertLogging
            |> Just
            |> UpdateLog.logResponse
        , UpdateFocus.focusInputPossibly model
        ]
    )


songsRecentResponseOk : Model -> HttpResponseText -> ElmCycle
songsRecentResponseOk model httpResponseText =
    case DecodeSongsRecent.decodeSongsRecentResponse httpResponseText of
        Err alertMessageTextDecode ->
            ( { model
                | alertMessageText =
                    alertMessageTextDecode
                        |> Alert.alertMessageTextSend Alert.actionDescriptionRecent
                        |> Just
                , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
              }
            , Cmd.batch
                [ (++)
                    alertMessageTextDecode
                    httpResponseText
                    |> Just
                    |> UpdateLog.logDecoding
                , UpdateFocus.focusInputPossibly model
                ]
            )

        Ok songsRecentNew ->
            ( { model
                | alertMessageText = Alert.alertMessageTextInit
                , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
                , songsRecent = songsRecentNew
              }
              --Here, don't log the full response.
            , Cmd.batch
                [ UpdateLog.logResponse Nothing
                , UpdateFocus.focusInputPossibly model
                ]
            )
