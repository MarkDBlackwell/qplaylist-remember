{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ResponseUpdate exposing
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
import ElmCycle
    exposing
        ( Msg(..)
        )
import FocusUpdate
import Http
import LikeOrCommentResponseDecode
import LogUpdate
import ModelInitialize
import ModelType
    exposing
        ( Model
        )
import RequestUpdateType
    exposing
        ( ActionLikeOrComment(..)
        , HttpResponseText
        )
import Song
import SongHelper
import SongInitialize
import SongType
    exposing
        ( SongRememberedMaybe
        , SongsRemembered
        )
import SongsRecentDecode
import UpdateHelper
import Utilities
    exposing
        ( cmdMsg2Cmd
        )



-- UPDATE


alertLogging : Http.Error -> AlertMessageText
alertLogging httpError =
    httpError
        |> Alert.messageTextErrorHttpLogging


likeOrCommentResponseErr : Model -> Http.Error -> ActionLikeOrComment -> ElmCycle.ElmCycle
likeOrCommentResponseErr model httpError actionLikeOrComment =
    let
        alertLikeOrComment : AlertType.LikeOrCommentName -> AlertMessageText
        alertLikeOrComment =
            httpError
                |> Alert.messageTextRequestLikeOrComment

        modelNewSongLikingOrCommentingOnNow : Model
        modelNewSongLikingOrCommentingOnNow =
            case actionLikeOrComment of
                Comment ->
                    model

                Like ->
                    { model
                        | songLikingNowMaybe = SongInitialize.songLikingNowMaybeInit
                    }
    in
    ( { modelNewSongLikingOrCommentingOnNow
        | alertMessageText =
            actionLikeOrComment
                |> UpdateHelper.actionLikeOrComment2String
                |> alertLikeOrComment
                |> Just
        , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
      }
    , Cmd.batch
        [ actionLikeOrComment
            |> UpdateHelper.actionLikeOrComment2String
            |> alertLikeOrComment
            |> String.append
                (httpError
                    |> alertLogging
                )
            |> Just
            |> LogUpdate.cmdLogResponse
        , FocusUpdate.cmdFocusInputPossibly model
        ]
    )


likeOrCommentResponseOk : Model -> HttpResponseText -> ActionLikeOrComment -> ElmCycle.ElmCycle
likeOrCommentResponseOk model httpResponseText actionLikeOrComment =
    let
        actionDescription : AlertMessageText
        actionDescription =
            actionLikeOrComment
                |> UpdateHelper.actionLikeOrComment2String
                |> String.append "send your "

        cmdButtonCommand : Cmd ElmCycle.Msg
        cmdButtonCommand =
            case actionLikeOrComment of
                Comment ->
                    Cmd.none

                Like ->
                    cmdButtonCommandAccomplished

        cmdButtonCommandAccomplished : Cmd ElmCycle.Msg
        cmdButtonCommandAccomplished =
            actionLikeOrComment
                |> UpdateHelper.actionLikeOrComment2String
                |> SongHelper.buttonIdReconstruct
                    model.songsRemembered
                    songLikingOrCommentingOnNowMaybe
                |> FocusUpdate.cmdFocusSetId

        modelNewCommentText : Model
        modelNewCommentText =
            case actionLikeOrComment of
                Comment ->
                    { modelNewSongLikingOrCommentingOnNow
                        | commentText = ModelInitialize.commentTextInit
                    }

                Like ->
                    modelNewSongLikingOrCommentingOnNow

        modelNewSongLikingOrCommentingOnNow : Model
        modelNewSongLikingOrCommentingOnNow =
            case actionLikeOrComment of
                Comment ->
                    { model
                        | songCommentingOnNowMaybe = SongInitialize.songCommentingOnNowMaybeInit
                    }

                Like ->
                    { model
                        | songLikingNowMaybe = SongInitialize.songLikingNowMaybeInit
                    }

        songLikingOrCommentingOnNowMaybe : SongRememberedMaybe
        songLikingOrCommentingOnNowMaybe =
            case actionLikeOrComment of
                Comment ->
                    model.songCommentingOnNowMaybe

                Like ->
                    model.songLikingNowMaybe
    in
    case LikeOrCommentResponseDecode.decodeLikeOrCommentResponse httpResponseText of
        Err alertMessageTextDecode ->
            ( { model
                | alertMessageText =
                    alertMessageTextDecode
                        |> Alert.messageTextSend actionDescription
                        |> Just
                , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
              }
            , Cmd.batch
                [ httpResponseText
                    |> String.append alertMessageTextDecode
                    |> Just
                    |> LogUpdate.cmdLogDecoding
                , cmdButtonCommand
                , FocusUpdate.cmdFocusInputPossibly model
                ]
            )

        Ok responseText ->
            let
                serverSaysRequestWasBad : Bool
                serverSaysRequestWasBad =
                    "good" /= responseText
            in
            if serverSaysRequestWasBad then
                ( { modelNewSongLikingOrCommentingOnNow
                    | alertMessageText =
                        responseText
                            |> Alert.messageTextSend actionDescription
                            |> Just
                    , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
                  }
                , Cmd.batch
                    [ httpResponseText
                        |> String.append responseText
                        |> Just
                        |> LogUpdate.cmdLogResponse
                    , cmdButtonCommand
                    , FocusUpdate.cmdFocusInputPossibly model
                    ]
                )

            else
                let
                    songsRememberedNew : SongsRemembered
                    songsRememberedNew =
                        model.songsRemembered
                            |> Song.likedOrCommentedShow
                                songLikingOrCommentingOnNowMaybe
                in
                ( { modelNewCommentText
                    | alertMessageText = Alert.messageTextInit
                    , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
                    , songsRemembered = songsRememberedNew
                  }
                , Cmd.batch
                    [ MsgSongsRememberedStore
                        |> cmdMsg2Cmd
                    , LogUpdate.cmdLogResponse Nothing
                    , cmdButtonCommandAccomplished
                    , FocusUpdate.cmdFocusInputPossibly model
                    ]
                )


songsRecentResponseErr : Model -> Http.Error -> ElmCycle.ElmCycle
songsRecentResponseErr model httpError =
    let
        alertMessageTextNew : AlertMessageText
        alertMessageTextNew =
            let
                alertScreen : String
                alertScreen =
                    httpError
                        |> Alert.messageTextErrorHttpScreen
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
            |> LogUpdate.cmdLogResponse
        , FocusUpdate.cmdFocusInputPossibly model
        ]
    )


songsRecentResponseOk : Model -> HttpResponseText -> ElmCycle.ElmCycle
songsRecentResponseOk model httpResponseText =
    case SongsRecentDecode.decodeSongsRecentResponse httpResponseText of
        Err alertMessageTextDecode ->
            ( { model
                | alertMessageText =
                    alertMessageTextDecode
                        |> Alert.messageTextSend Alert.actionDescriptionRecent
                        |> Just
                , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
              }
            , Cmd.batch
                [ httpResponseText
                    |> String.append alertMessageTextDecode
                    |> Just
                    |> LogUpdate.cmdLogDecoding
                , FocusUpdate.cmdFocusInputPossibly model
                ]
            )

        Ok songsRecentNew ->
            ( { model
                | alertMessageText = Alert.messageTextInit
                , awaitingServerResponse = ModelInitialize.awaitingServerResponseInit
                , songsRecent = songsRecentNew
              }
              --Here, don't log the full response.
            , Cmd.batch
                [ LogUpdate.cmdLogResponse Nothing
                , FocusUpdate.cmdFocusInputPossibly model
                ]
            )
