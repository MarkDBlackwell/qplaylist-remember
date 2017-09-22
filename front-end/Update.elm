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


module Update
    exposing
        ( update
        )

import Alert
    exposing
        ( AlertMessageText
        , alertMessageTextErrorHttpLogging
        , alertMessageTextErrorHttpScreen
        , alertMessageTextErrorUnexpected
        , alertMessageTextInit
        , alertMessageTextRequestLikeOrComment
        , alertMessageTextServerAwaiting
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
        ( Request
        , getString
        , send
        )
import Initialize
    exposing
        ( awaitingServerResponseInit
        , commentTextInit
        )
import MessageType
    exposing
        ( Msg(..)
        )
import ModelType
    exposing
        ( Model
        , PageIsExpanded
        )
import Request
    exposing
        ( AwaitingServerResponse
        , HttpRequestText
        , UriText
        , likeOrCommentRequestUriText
        , relative
        )
import Song
    exposing
        ( SongCommenting
        , SongLikingOrCommenting
        , SongsRemembered
        , likedOrCommentedShow
        , songCommentingInit
        , songLikingInit
        , songLikingOrCommentingMaybe
        , songsRememberedAppendOneUnique
        , songsRememberedWithoutOne
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        , focusSet
        , updateFocusResult
        , updateFocusSet
        )
import UpdateLog
    exposing
        ( updateHttpRequestOrResponseTextLog
        )
import UpdateResponse
    exposing
        ( updateCommentResponseErr
        , updateCommentResponseOk
        , updateLikeResponseErr
        )
import UpdateType
    exposing
        ( Optional
            ( Closed
            , Open
            )
        )
import UpdateUtilities
    exposing
        ( msg2Cmd
        )
import UserIdentifier
    exposing
        ( UserIdentifier
        , keyCode2Char
        , threeDigits
        )


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        stateVector : ( AwaitingServerResponse, Optional )
        stateVector =
            let
                commentOptional : Optional
                commentOptional =
                    case model.songCommenting of
                        Nothing ->
                            Closed

                        Just songCommenting ->
                            Open
            in
            ( model.awaitingServerResponse
            , commentOptional
            )
    in
    case msg of
        CommentAreaInputTextChangeCaptureHand text ->
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | commentText = text
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , commentText = text
                      }
                    , Cmd.none
                    )

        CommentAreaOpenHand songsRememberedIndex ->
            let
                songCommentingNew : SongLikingOrCommenting
                songCommentingNew =
                    songLikingOrCommentingMaybe model.songsRemembered songsRememberedIndex
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextServerAwaiting
                      }
                    , focusInputPossibly model
                    )

                ( _, Open ) ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , commentText = commentTextInit
                        , songCommenting = songCommentingNew
                      }
                      --'focusInputPossibly' doesn't work, here:
                    , focusSet "input"
                    )

        CommentCancelHand ->
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextServerAwaiting
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , commentText = commentTextInit
                        , songCommenting = songCommentingInit
                      }
                    , Cmd.none
                    )

        CommentResponse (Err httpError) ->
            updateCommentResponseErr model httpError

        CommentResponse (Ok httpResponseText) ->
            updateCommentResponseOk model httpResponseText

        CommentSendHand ->
            let
                commentRequest : Cmd Msg
                commentRequest =
                    send CommentResponse (getString commentRequestUriText)

                commentRequestUriText : UriText
                commentRequestUriText =
                    likeOrCommentRequestUriText model.songCommenting model.userIdentifier model.commentText
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextServerAwaiting
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    if String.isEmpty model.commentText then
                        ( { model
                            | alertMessageText = alertMessageTextInit
                          }
                        , focusInputPossibly model
                        )
                    else
                        ( { model
                            | alertMessageText = alertMessageTextInit
                            , awaitingServerResponse = True
                          }
                        , Cmd.batch
                            [ msg2Cmd (HttpRequestOrResponseTextLog "Request" commentRequestUriText)
                            , commentRequest
                            , focusInputPossibly model
                            ]
                        )

        FocusResult _ ->
            updateFocusResult model

        FocusSet id ->
            updateFocusSet model id

        HttpRequestOrResponseTextLog requestOrResponseLabelText httpRequestOrResponseText ->
            updateHttpRequestOrResponseTextLog model requestOrResponseLabelText httpRequestOrResponseText

        InitialSetUp threeLetterSpace ->
            let
                userIdentifierNew : UserIdentifier
                userIdentifierNew =
                    String.fromList (List.map keyCode2Char (threeDigits threeLetterSpace))
            in
            ( { model
                | userIdentifier = userIdentifierNew
              }
            , Cmd.none
            )

        LikeButtonProcessHand songsRememberedIndex ->
            let
                likeRequest : Cmd Msg
                likeRequest =
                    send LikeResponse (getString likeRequestUriText)

                likeRequestUriText : UriText
                likeRequestUriText =
                    likeOrCommentRequestUriText songLikingNew model.userIdentifier "Loved it!"

                songLikingNew : SongLikingOrCommenting
                songLikingNew =
                    songLikingOrCommentingMaybe model.songsRemembered songsRememberedIndex
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextServerAwaiting
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , awaitingServerResponse = True
                        , songLiking = songLikingNew
                      }
                    , Cmd.batch
                        [ msg2Cmd (HttpRequestOrResponseTextLog "Request" likeRequestUriText)
                        , likeRequest
                        , focusInputPossibly model
                        ]
                    )

        LikeResponse (Err httpError) ->
            updateLikeResponseErr model httpError

        LikeResponse (Ok appendLikeJson) ->
            case decodeLikeOrCommentResponse appendLikeJson of
                Err alertMessageTextDecode ->
                    let
                        alertMessageTextNew : AlertMessageText
                        alertMessageTextNew =
                            alertMessageTextErrorUnexpected
                                [ "while attempting to send your Like"
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
                                    [ "while attempting to send your Like"
                                    , responseString
                                    ]
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

        PageMorphHand ->
            let
                pageIsExpandedNew : PageIsExpanded
                pageIsExpandedNew =
                    --Here, can't use List.all.
                    if
                        List.isEmpty model.songsLatest
                            && List.isEmpty model.songsRemembered
                    then
                        model.pageIsExpanded
                    else
                        not model.pageIsExpanded
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextServerAwaiting
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , pageIsExpanded = pageIsExpandedNew
                      }
                    , focusInputPossibly model
                    )

        SongBuyAnchorProcessHand ->
            ( model
            , focusInputPossibly model
            )

        SongForgetHand songsRememberedIndex ->
            let
                songRememberedCompare : SongCommenting
                songRememberedCompare =
                    songLikingOrCommentingMaybe model.songsRemembered songsRememberedIndex

                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    songsRememberedWithoutOne model.songsRemembered songsRememberedIndex
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextServerAwaiting
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    if model.songCommenting == songRememberedCompare then
                        ( { model
                            | alertMessageText = alertMessageTextInit
                          }
                        , focusInputPossibly model
                        )
                    else
                        ( { model
                            | alertMessageText = alertMessageTextInit
                            , songsRemembered = songsRememberedNew
                          }
                        , focusInputPossibly model
                        )

        SongRememberHand songsLatestIndex ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    songsRememberedAppendOneUnique model.songsLatest songsLatestIndex model.songsRemembered
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextServerAwaiting
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , songsRemembered = songsRememberedNew
                      }
                    , focusInputPossibly model
                    )

        SongsLatestRefreshHand ->
            let
                requestUriText : UriText
                requestUriText =
                    let
                        basename : UriText
                        basename =
                            "LatestFive.json"

                        subUri : UriText
                        subUri =
                            "wtmdapp"
                    in
                    relative
                        [ ".."
                        , subUri
                        , basename
                        ]
                        []

                songsLatestRequest : Cmd Msg
                songsLatestRequest =
                    let
                        request : Request HttpRequestText
                        request =
                            getString requestUriText
                    in
                    send SongsLatestResponse request
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextServerAwaiting
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , awaitingServerResponse = True
                      }
                    , Cmd.batch
                        [ msg2Cmd (HttpRequestOrResponseTextLog "Request" requestUriText)
                        , songsLatestRequest
                        , focusInputPossibly model
                        ]
                    )

        SongsLatestResponse (Err httpError) ->
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

        SongsLatestResponse (Ok jsonRawText) ->
            case decodeSongsLatest jsonRawText of
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
