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
import UpdateFocus
    exposing
        ( focusInputPossibly
        )
import UpdateUtilities
    exposing
        ( msg2Cmd
        )


-- UPDATE


alertMessageTextSend : AlertMessageText -> AlertMessageText -> AlertMessageText
alertMessageTextSend action details =
    alertMessageTextErrorUnexpected
        [ "while attempting to "
            ++ action
        , details
        ]


updateCommentResponseErr : Model -> Error -> ( Model, Cmd Msg )
updateCommentResponseErr model httpError =
    ( { model
        | alertMessageText = alertMessageTextRequestLikeOrComment httpError "comment"
        , awaitingServerResponse = awaitingServerResponseInit
      }
    , Cmd.batch
        [ msg2Cmd (HttpRequestOrResponseTextLog "Response" (alertMessageTextErrorHttpLogging httpError))
        , focusInputPossibly model
        ]
    )


updateCommentResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
updateCommentResponseOk model httpResponseText =
    case decodeLikeOrCommentResponse httpResponseText of
        Err alertMessageTextDecode ->
            let
                alertMessageTextNew : AlertMessageText
                alertMessageTextNew =
                    alertMessageTextErrorUnexpected
                        [ "while attempting to send your Comment"
                        , alertMessageTextDecode
                        ]
            in
            ( { model
                | alertMessageText = alertMessageTextNew
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Cmd.batch
                [ msg2Cmd (HttpRequestOrResponseTextLog "Decoding" alertMessageTextDecode)
                , focusInputPossibly model
                ]
            )

        Ok responseString ->
            if "ok" /= responseString then
                let
                    alertMessageTextNew : AlertMessageText
                    alertMessageTextNew =
                        alertMessageTextErrorUnexpected
                            [ "while attempting to send your Comment"
                            , responseString
                            ]
                in
                ( { model
                    | alertMessageText = alertMessageTextNew
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , Cmd.batch
                    [ msg2Cmd (HttpRequestOrResponseTextLog "Response" responseString)
                    , focusInputPossibly model
                    ]
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
                , msg2Cmd (HttpRequestOrResponseTextLog "Response" "")
                )


updateLikeResponseErr : Model -> Error -> ( Model, Cmd Msg )
updateLikeResponseErr model httpError =
    ( { model
        | alertMessageText = alertMessageTextRequestLikeOrComment httpError "Like"
        , awaitingServerResponse = awaitingServerResponseInit
        , songLiking = songLikingInit
      }
    , Cmd.batch
        [ msg2Cmd (HttpRequestOrResponseTextLog "Response" (alertMessageTextErrorHttpLogging httpError))
        , focusInputPossibly model
        ]
    )


updateLikeResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
updateLikeResponseOk model httpResponseText =
    case decodeLikeOrCommentResponse httpResponseText of
        Err alertMessageTextDecode ->
            let
                alertMessageTextNew : AlertMessageText
                alertMessageTextNew =
                    alertMessageTextSend "send your Like" alertMessageTextDecode
            in
            ( { model
                | alertMessageText = alertMessageTextNew
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Cmd.batch
                [ msg2Cmd (HttpRequestOrResponseTextLog "Decoding" alertMessageTextDecode)
                , focusInputPossibly model
                ]
            )

        Ok responseString ->
            if "ok" /= responseString then
                let
                    alertMessageTextNew : AlertMessageText
                    alertMessageTextNew =
                        alertMessageTextSend "send your Like" responseString
                in
                ( { model
                    | alertMessageText = alertMessageTextNew
                    , awaitingServerResponse = awaitingServerResponseInit
                    , songLiking = songLikingInit
                  }
                , Cmd.batch
                    [ msg2Cmd (HttpRequestOrResponseTextLog "Response" responseString)
                    , focusInputPossibly model
                    ]
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
                , Cmd.batch
                    [ msg2Cmd (HttpRequestOrResponseTextLog "Response" "")
                    , focusInputPossibly model
                    ]
                )


updateSongsLatestResponseErr : Model -> Error -> ( Model, Cmd Msg )
updateSongsLatestResponseErr model httpError =
    let
        alertMessageTextNew : AlertMessageText
        alertMessageTextNew =
            alertMessageTextErrorHttpScreen httpError
                ++ " (while attempting to access the latest few songs)"
    in
    ( { model
        | alertMessageText = alertMessageTextNew
        , awaitingServerResponse = awaitingServerResponseInit
      }
    , Cmd.batch
        [ msg2Cmd (HttpRequestOrResponseTextLog "Response" (alertMessageTextErrorHttpLogging httpError))
        , focusInputPossibly model
        ]
    )


updateSongsLatestResponseOk : Model -> HttpResponseText -> ( Model, Cmd Msg )
updateSongsLatestResponseOk model httpResponseText =
    case decodeSongsLatest httpResponseText of
        Err alertMessageTextDecode ->
            let
                alertMessageTextNew : AlertMessageText
                alertMessageTextNew =
                    alertMessageTextErrorUnexpected
                        [ "while attempting to access the latest few songs"
                        , alertMessageTextDecode
                        ]
            in
            ( { model
                | alertMessageText = alertMessageTextNew
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Cmd.batch
                [ msg2Cmd (HttpRequestOrResponseTextLog "Decoding" alertMessageTextDecode)
                , focusInputPossibly model
                ]
            )

        Ok songsLatestNew ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , awaitingServerResponse = awaitingServerResponseInit
                , songsLatest = songsLatestNew
              }
            , Cmd.batch
                --Here, don't log the full response.
                [ msg2Cmd (HttpRequestOrResponseTextLog "Response" "")
                , focusInputPossibly model
                ]
            )
