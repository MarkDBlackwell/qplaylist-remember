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

import Debug
    exposing
        ( log
        )
import DecodeLikeOrCommentResponse
    exposing
        ( decodeLikeOrCommentResponse
        )
import DecodeSongsBasic
    exposing
        ( decodeSongsBasic
        )
import Dom
    exposing
        ( focus
        )
import Http
    exposing
        ( Request
        , getString
        , send
        )
import MessageDetails
    exposing
        ( Msg(..)
        )
import ModelDetails
    exposing
        ( AlertMessageText
        , AwaitingServerResponse
        , CommentText
        , Model
        , Optional
            ( Closed
            , Open
            )
        , PageIsExpanded
        )
import ModelDetailsUpdate
    exposing
        ( AlertMessageOptional
        , DecodeErrorMessageText
        , HttpErrorMessageText
        , HttpRequestText
        , LikeText
        , UriText
        )
import ModelInitialize
    exposing
        ( alertMessageTextInit
        , awaitingServerResponseInit
        , commentTextInit
        )
import Song
    exposing
        ( SongBasic
        , SongCommenting
        , SongLatestFew
        , SongLiking
        , SongLikingOrCommenting
        , SongRemembered
        , SongRememberedIndex
        , SongsLatestFew
        , SongsRemembered
        , songBasic2SongRemembered
        , songCommentingInit
        , songLikingInit
        , songLikingOrCommentingNew
        , songRemembered2SongBasic
        )
import Task
    exposing
        ( attempt
        , succeed
        )
import UpdateDetails
    exposing
        ( alertMessageTextLikeOrCommentRequest
        , alertMessageTextUnexpectedError
        , focusInputPossibly
        , likeOrCommentRequestUriText
        , likedOrCommentedShow
        , relative
        )
import UpdateUtilities
    exposing
        ( focusSet
        , httpErrorMessageLogging
        , httpErrorMessageScreen
        , msg2Cmd
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
        alertMessageTextAwaitingServer : AlertMessageText
        alertMessageTextAwaitingServer =
            "Awaiting server"

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

        CommentAreaOpenHand songRememberedIndex ->
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextAwaitingServer
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
                        , songCommenting = songLikingOrCommentingNew model.songsRemembered songRememberedIndex
                      }
                      --'focusInputPossibly' doesn't work, here:
                    , focusSet "input"
                    )

        CommentCancelHand ->
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextAwaitingServer
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
            ( { model
                | alertMessageText = alertMessageTextLikeOrCommentRequest httpError "comment"
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Cmd.batch
                [ msg2Cmd (HttpRequestOrResponseTextLog "Response" (httpErrorMessageLogging httpError))
                , focusInputPossibly model
                ]
            )

        CommentResponse (Ok appendCommentJson) ->
            case decodeLikeOrCommentResponse appendCommentJson of
                Err decodeErrorMessageText ->
                    let
                        alertMessageTextNew : AlertMessageText
                        alertMessageTextNew =
                            alertMessageTextUnexpectedError
                                "while attempting to append your Comment"
                                decodeErrorMessageText
                    in
                    ( { model
                        | alertMessageText = alertMessageTextNew
                        , awaitingServerResponse = awaitingServerResponseInit
                      }
                    , Cmd.batch
                        [ msg2Cmd (HttpRequestOrResponseTextLog "Decoding" decodeErrorMessageText)
                        , focusInputPossibly model
                        ]
                    )

                Ok responseString ->
                    if "ok" /= responseString then
                        let
                            alertMessageTextNew : AlertMessageText
                            alertMessageTextNew =
                                alertMessageTextUnexpectedError
                                    "while attempting to send your Like"
                                    responseString
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
                                List.map (likedOrCommentedShow model.songCommenting) model.songsRemembered
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
                        | alertMessageText = alertMessageTextAwaitingServer
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
            ( model
            , Cmd.none
            )

        FocusSet id ->
            --See:
            --https://www.reddit.com/r/elm/comments/53y6s4/focus_on_input_box_after_clicking_button/
            --https://stackoverflow.com/a/39419640/1136063
            ( model
            , attempt FocusResult (focus id)
            )

        HttpRequestOrResponseTextLog labelText httpRequestOrResponseText ->
            let
                --Keep for console logging:
                a : String
                a =
                    if String.isEmpty httpRequestOrResponseText then
                        log labelText "Ok"
                    else
                        log labelText httpRequestOrResponseText
            in
            ( model
            , focusInputPossibly model
            )

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

        LikeButtonProcessHand songRememberedIndex ->
            let
                likeRequest : Cmd Msg
                likeRequest =
                    send LikeResponse (getString likeRequestUriText)

                likeRequestUriText : UriText
                likeRequestUriText =
                    likeOrCommentRequestUriText songLikingNew model.userIdentifier "Loved it!"

                songLikingNew : SongLikingOrCommenting
                songLikingNew =
                    songLikingOrCommentingNew model.songsRemembered songRememberedIndex
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextAwaitingServer
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
            ( { model
                | alertMessageText = alertMessageTextLikeOrCommentRequest httpError "Like"
                , awaitingServerResponse = awaitingServerResponseInit
                , songLiking = songLikingInit
              }
            , Cmd.batch
                [ msg2Cmd (HttpRequestOrResponseTextLog "Response" (httpErrorMessageLogging httpError))
                , focusInputPossibly model
                ]
            )

        LikeResponse (Ok appendLikeJson) ->
            case decodeLikeOrCommentResponse appendLikeJson of
                Err decodeErrorMessageText ->
                    let
                        alertMessageTextNew : AlertMessageText
                        alertMessageTextNew =
                            alertMessageTextUnexpectedError
                                "while attempting to append your Like"
                                decodeErrorMessageText
                    in
                    ( { model
                        | alertMessageText = alertMessageTextNew
                        , awaitingServerResponse = awaitingServerResponseInit
                      }
                    , Cmd.batch
                        [ msg2Cmd (HttpRequestOrResponseTextLog "Decoding" decodeErrorMessageText)
                        , focusInputPossibly model
                        ]
                    )

                Ok responseString ->
                    if "ok" /= responseString then
                        let
                            alertMessageTextNew : AlertMessageText
                            alertMessageTextNew =
                                alertMessageTextUnexpectedError
                                    "while attempting to send your Like"
                                    responseString
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
                                List.map (likedOrCommentedShow model.songLiking) model.songsRemembered
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
                        List.isEmpty model.songsLatestFew
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
                        | alertMessageText = alertMessageTextAwaitingServer
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

        SongForgetHand songRememberedIndex ->
            let
                songRememberedCompare : SongsRemembered -> Maybe SongBasic
                songRememberedCompare songsRemembered =
                    let
                        songRememberedSelected : SongsRemembered -> SongRememberedIndex -> Maybe SongRemembered
                        songRememberedSelected songsRemembered songRememberedIndex =
                            List.head (List.drop songRememberedIndex songsRemembered)
                    in
                    case songRememberedSelected songsRemembered songRememberedIndex of
                        Nothing ->
                            Nothing

                        Just songRememberedSelected ->
                            Just (songRemembered2SongBasic songRememberedSelected)

                songsRememberedWithoutOne : SongsRemembered
                songsRememberedWithoutOne =
                    List.take songRememberedIndex model.songsRemembered
                        ++ List.drop (songRememberedIndex + 1) model.songsRemembered
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextAwaitingServer
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    if model.songCommenting == songRememberedCompare model.songsRemembered then
                        ( { model
                            | alertMessageText = alertMessageTextInit
                          }
                        , focusInputPossibly model
                        )
                    else
                        ( { model
                            | alertMessageText = alertMessageTextInit
                            , songsRemembered = songsRememberedWithoutOne
                          }
                        , focusInputPossibly model
                        )

        SongRememberHand songLatestFewIndex ->
            let
                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    let
                        songLatestFewSelected : Maybe SongLatestFew
                        songLatestFewSelected =
                            List.head (List.drop songLatestFewIndex model.songsLatestFew)
                    in
                    case songLatestFewSelected of
                        Nothing ->
                            model.songsRemembered

                        Just songLatestFewSelected ->
                            if
                                List.member
                                    songLatestFewSelected
                                    (List.map songRemembered2SongBasic model.songsRemembered)
                            then
                                model.songsRemembered
                            else
                                model.songsRemembered
                                    ++ [ songBasic2SongRemembered songLatestFewSelected ]
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextAwaitingServer
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

        SongsLatestFewRefreshHand ->
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

                songsLatestFewRequest : Cmd Msg
                songsLatestFewRequest =
                    let
                        request : Request HttpRequestText
                        request =
                            getString requestUriText
                    in
                    send SongsLatestFewResponse request
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextAwaitingServer
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
                        , songsLatestFewRequest
                        , focusInputPossibly model
                        ]
                    )

        SongsLatestFewResponse (Err httpError) ->
            let
                alertMessageTextNew : AlertMessageText
                alertMessageTextNew =
                    httpErrorMessageScreen httpError
                        ++ " (while attempting to access the latest few songs)"
            in
            ( { model
                | alertMessageText = alertMessageTextNew
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Cmd.batch
                [ msg2Cmd (HttpRequestOrResponseTextLog "Response" (httpErrorMessageLogging httpError))
                , focusInputPossibly model
                ]
            )

        SongsLatestFewResponse (Ok jsonRawText) ->
            case decodeSongsBasic jsonRawText of
                Err decodeErrorMessageText ->
                    let
                        alertMessageTextNew : AlertMessageText
                        alertMessageTextNew =
                            alertMessageTextUnexpectedError
                                "while attempting to access the latest few songs"
                                decodeErrorMessageText
                    in
                    ( { model
                        | alertMessageText = alertMessageTextNew
                        , awaitingServerResponse = awaitingServerResponseInit
                      }
                    , Cmd.batch
                        [ msg2Cmd (HttpRequestOrResponseTextLog "Decoding" decodeErrorMessageText)
                        , focusInputPossibly model
                        ]
                    )

                Ok songsLatestFewNew ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , awaitingServerResponse = awaitingServerResponseInit
                        , songsLatestFew = songsLatestFewNew
                      }
                      --Here, don't log the full response.
                    , Cmd.batch
                        [ msg2Cmd (HttpRequestOrResponseTextLog "Response" "")
                        , focusInputPossibly model
                        ]
                    )
