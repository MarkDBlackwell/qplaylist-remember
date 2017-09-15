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


module Update exposing (update)

import Debug exposing (log)
import DecodeSongsBasic exposing (decodeSongsBasic)
import Dom exposing (focus)
import Http
    exposing
        ( Error
        , Request
        , getString
        , send
        )
import MessageDetails exposing (Msg(..))
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
        , SongBasic
        , SongCommenting
        , SongLatestFew
        , SongLiking
        , SongLikingOrCommenting
        , SongRemembered
        , SongsLatestFew
        , SongsRemembered
        , songBasic2SongRemembered
        , songRemembered2SongBasic
        )
import ModelDetailsUpdate
    exposing
        ( AlertMessageOptional
        , HttpErrorMessageText
        , HttpRequestText
        , LikeText
        , SongRememberedIndex
        , UriText
        )
import ModelInitialize
    exposing
        ( alertMessageTextInit
        , awaitingServerResponseInit
        , commentTextInit
        , songCommentingInit
        , songLikingInit
        )
import Task
    exposing
        ( attempt
        , succeed
        )
import UpdateDetails
    exposing
        ( focusInputPossibly
        , relative
        )
import UpdateUtilities
    exposing
        ( focusSet
        , httpErrorMessageLogging
        , httpErrorMessageScreen
        , msg2Cmd
        )


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        alertMessageTextLikeOrComment : Error -> String -> AlertMessageText
        alertMessageTextLikeOrComment httpError likeOrCommentName =
            httpErrorMessageScreen httpError
                ++ " (while attempting to send "
                ++ likeOrCommentName
                ++ " to server)"

        commentOptional : Optional
        commentOptional =
            case model.songCommenting of
                Nothing ->
                    Closed

                Just songCommenting ->
                    Open

        likeOrCommentRequestUriText : SongLikingOrCommenting -> String -> UriText
        likeOrCommentRequestUriText songLikingOrCommenting likeOrCommentText =
            let
                artistTimeTitle : UriText
                artistTimeTitle =
                    case songLikingOrCommenting of
                        Nothing ->
                            ""

                        Just songLikingOrCommenting ->
                            songLikingOrCommenting.time
                                ++ " "
                                ++ songLikingOrCommenting.artist
                                ++ ": "
                                ++ songLikingOrCommenting.title

                basename : UriText
                basename =
                    "append.php"

                timeStamp : UriText
                timeStamp =
                    case songLikingOrCommenting of
                        Nothing ->
                            ""

                        Just songLikingOrCommenting ->
                            songLikingOrCommenting.timeStamp
            in
            relative
                [ basename ]
                [ ( "timestamp", timeStamp )
                , ( "song", artistTimeTitle )
                , ( "comment", likeOrCommentText )
                ]

        likedOrCommentedShow : SongLikingOrCommenting -> SongRemembered -> SongRemembered
        likedOrCommentedShow songLikingOrCommenting song =
            case songLikingOrCommenting of
                Nothing ->
                    song

                Just songLikingOrCommenting ->
                    if songLikingOrCommenting /= songRemembered2SongBasic song then
                        song
                    else
                        { song
                            | likedOrCommented = True
                        }

        songLikingOrCommentingNew : SongRememberedIndex -> SongLikingOrCommenting
        songLikingOrCommentingNew songRememberedIndex =
            case List.head (List.drop songRememberedIndex model.songsRemembered) of
                Nothing ->
                    Nothing

                Just songSelected ->
                    Just (songRemembered2SongBasic songSelected)

        stateVector : ( AwaitingServerResponse, Optional )
        stateVector =
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
                    ( model
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
                        , songCommenting = songLikingOrCommentingNew songRememberedIndex
                      }
                      --'focusInputPossibly' doesn't work, here:
                    , focusSet "input"
                    )

        CommentCancelHand ->
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( model
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
                | alertMessageText = alertMessageTextLikeOrComment httpError "comment"
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Cmd.batch
                [ msg2Cmd (succeed (HttpResponseTextLog (httpErrorMessageLogging httpError)))
                , focusInputPossibly model
                ]
            )

        CommentResponse (Ok appendCommentJson) ->
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
            , msg2Cmd (succeed (HttpResponseTextLog appendCommentJson))
            )

        CommentSendHand ->
            let
                commentRequest : Cmd Msg
                commentRequest =
                    send CommentResponse (getString (log "Request" (likeOrCommentRequestUriText model.songCommenting model.commentText)))
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( model
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
                            [ commentRequest
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

        HttpResponseTextLog httpResponseText ->
            let
                --Keep for console logging:
                a : String
                a =
                    if String.isEmpty httpResponseText then
                        log "Response" "Ok"
                    else
                        log "Response" httpResponseText
            in
            ( model
            , focusInputPossibly model
            )

        LikeButtonProcessHand songRememberedIndex ->
            let
                likeRequest : Cmd Msg
                likeRequest =
                    send LikeResponse (getString (log "Request" (likeOrCommentRequestUriText songLikingNew "Loved it!")))

                songLikingNew : SongLikingOrCommenting
                songLikingNew =
                    songLikingOrCommentingNew songRememberedIndex
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( model
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , awaitingServerResponse = True
                        , songLiking = songLikingNew
                      }
                    , Cmd.batch
                        [ likeRequest
                        , focusInputPossibly model
                        ]
                    )

        LikeResponse (Err httpError) ->
            ( { model
                | alertMessageText = alertMessageTextLikeOrComment httpError "Like"
                , awaitingServerResponse = awaitingServerResponseInit
                , songLiking = songLikingInit
              }
            , focusInputPossibly model
            )

        LikeResponse (Ok appendLikeJson) ->
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
                [ msg2Cmd (succeed (HttpResponseTextLog appendLikeJson))
                , focusInputPossibly model
                ]
            )

        PageMorphHand ->
            let
                pageIsExpandedNew : PageIsExpanded
                pageIsExpandedNew =
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
                    ( model
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
                songRemembered : Maybe SongRemembered
                songRemembered =
                    List.head (List.drop songRememberedIndex model.songsRemembered)

                songRememberedCompare : Maybe SongBasic
                songRememberedCompare =
                    case songRemembered of
                        Nothing ->
                            Nothing

                        Just songRemembered ->
                            Just (songRemembered2SongBasic songRemembered)

                songsRememberedWithoutOne : SongsRemembered
                songsRememberedWithoutOne =
                    List.take songRememberedIndex model.songsRemembered
                        ++ List.drop (songRememberedIndex + 1) model.songsRemembered
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( model
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
                            , songsRemembered = songsRememberedWithoutOne
                          }
                        , focusInputPossibly model
                        )

        SongRememberHand songLatestFewIndex ->
            let
                songLatestFewSelected : Maybe SongLatestFew
                songLatestFewSelected =
                    List.head (List.drop songLatestFewIndex model.songsLatestFew)

                songsRememberedNew : SongsRemembered
                songsRememberedNew =
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
                    ( model
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
                basename : UriText
                basename =
                    "LatestFive.json"

                request : Request HttpRequestText
                request =
                    getString (log "LatestFew" requestUriText)

                requestUriText : UriText
                requestUriText =
                    relative
                        [ ".."
                        , subUri
                        , basename
                        ]
                        []

                songsLatestFewRequest : Cmd Msg
                songsLatestFewRequest =
                    send SongsLatestFewResponse request

                subUri : UriText
                subUri =
                    "wtmdapp"
            in
            --(awaitingServer, commentArea)
            case stateVector of
                ( True, _ ) ->
                    ( model
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , awaitingServerResponse = True
                      }
                    , Cmd.batch
                        [ songsLatestFewRequest
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
            , focusInputPossibly model
            )

        SongsLatestFewResponse (Ok jsonRawText) ->
            let
                songsLatestFewNew : SongsLatestFew
                songsLatestFewNew =
                    decodeSongsBasic jsonRawText
            in
            ( { model
                | alertMessageText = alertMessageTextInit
                , awaitingServerResponse = awaitingServerResponseInit
                , songsLatestFew = songsLatestFewNew
              }
            , focusInputPossibly model
            )
