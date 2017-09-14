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
        ( Request
        , getString
        , send
        )
import MessageDetails exposing (Msg(..))
import ModelDetails
    exposing
        ( AlertMessageText
        , AwaitingServerResponse
        , CommentAreaOptional
        , CommentText
        , Model
        , Optional
            ( Closed
            , Open
            )
        , PageIsExpanded
        , SongCommenting
        , SongLatestFew
        , SongLiking
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
        , commentAreaOptionalInit
        , commentTextInit
        , processingCommentInit
        , processingLikeInit
        , songCommentingIndexInit
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
        , likeOrCommentRequestUriText
        , likeResponse
        , likingOrCommenting
        )
import UpdateUtilities
    exposing
        ( focusSet
        , httpErrorMessageLogging
        , httpErrorMessageScreen
        , msg2Cmd
        )
import ViewUtilities exposing (relative)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        songLikingOrCommentingNew : SongRememberedIndex -> SongCommenting
        songLikingOrCommentingNew songRememberedIndex =
            case List.head (List.drop songRememberedIndex model.songsRemembered) of
                Nothing ->
                    Nothing

                Just songSelected ->
                    Just (songRemembered2SongBasic songSelected)

        stateVector : ( AlertMessageOptional, AwaitingServerResponse, CommentAreaOptional )
        stateVector =
            let
                alertMessageOptional : AlertMessageOptional
                alertMessageOptional =
                    if model.alertMessageText == alertMessageTextInit then
                        Closed
                    else
                        Open
            in
            ( alertMessageOptional
            , model.awaitingServerResponse
            , model.commentAreaOptional
            )
    in
    case msg of
        CommentAreaInputTextChangeCaptureHand text ->
            --(alertMessage, awaitingServer, commentArea)
            case stateVector of
                ( _, True, _ ) ->
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
            --(alertMessage, awaitingServer, commentArea)
            case stateVector of
                ( _, True, _ ) ->
                    ( model
                    , focusInputPossibly model
                    )

                ( _, _, Open ) ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , commentAreaOptional = Open
                        , commentText = commentTextInit
                        , processingComment = True
                        , songCommenting = songLikingOrCommentingNew songRememberedIndex
                      }
                      --'focusInputPossibly' doesn't work, here:
                    , focusSet "input"
                    )

        CommentCancelHand ->
            --(alertMessage, awaitingServer, commentArea)
            case stateVector of
                ( _, True, _ ) ->
                    ( model
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , commentAreaOptional = Closed
                        , commentText = commentTextInit
                        , processingComment = processingCommentInit
                        , songCommentingIndex = songCommentingIndexInit
                      }
                    , Cmd.none
                    )

        CommentResponse (Err httpError) ->
            let
                alertMessageTextNew : AlertMessageText
                alertMessageTextNew =
                    httpErrorMessageScreen httpError
                        ++ " (while attempting to send comment to server)"
            in
            ( { model
                | alertMessageText = alertMessageTextNew
                , awaitingServerResponse = awaitingServerResponseInit
              }
            , Cmd.batch
                [ msg2Cmd (succeed (HttpResponseTextLog (httpErrorMessageLogging httpError)))
                , focusInputPossibly model
                ]
            )

        CommentResponse (Ok appendCommentJson) ->
            let
                commentedShow : SongCommenting -> SongRemembered -> SongRemembered
                commentedShow songCommenting song =
                    case songCommenting of
                        Nothing ->
                            song

                        Just songCommenting ->
                            if songCommenting /= songRemembered2SongBasic song then
                                song
                            else
                                { song
                                    | likedOrCommented = True
                                }

                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    List.map (commentedShow model.songCommenting) model.songsRemembered
            in
            ( { model
                | alertMessageText = alertMessageTextInit
                , awaitingServerResponse = awaitingServerResponseInit
                , commentAreaOptional = commentAreaOptionalInit
                , commentText = commentTextInit
                , processingComment = processingCommentInit
                , songCommenting = songCommentingInit
                , songsRemembered = songsRememberedNew
              }
            , msg2Cmd (succeed (HttpResponseTextLog appendCommentJson))
            )

        CommentSendHand ->
            let
                commentRequest : Cmd Msg
                commentRequest =
                    send CommentResponse (getString (log "Request" (likeOrCommentRequestUriText model commentText)))

                commentText : CommentText
                commentText =
                    model.commentText
            in
            --(alertMessage, awaitingServer, commentArea)
            case stateVector of
                ( _, True, _ ) ->
                    ( model
                    , focusInputPossibly model
                    )

                _ ->
                    if String.isEmpty model.commentText then
                        ( { model
                            | alertMessageText = alertMessageTextInit
                            , awaitingServerResponse = awaitingServerResponseInit
                          }
                        , focusInputPossibly model
                        )
                    else
                        ( { model
                            | awaitingServerResponse = True
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
                        log "Response: Ok" httpResponseText
            in
            ( model
            , focusInputPossibly model
            )

        LikeButtonProcessHand songRememberedIndex ->
            let
                likeText : LikeText
                likeText =
                    "Loved it!"
            in
            --(alertMessage, awaitingServer, commentArea)
            case stateVector of
                ( _, True, _ ) ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                      }
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | awaitingServerResponse = True
                        , processingLike = True
                        , songLiking = songLikingOrCommentingNew songRememberedIndex
                      }
                    , Cmd.batch
                        [ send LikeResponse (getString (log "Request" (likeOrCommentRequestUriText model likeText)))
                        , focusInputPossibly model
                        ]
                    )

        LikeResponse (Err httpError) ->
            let
                alertMessageTextNew : AlertMessageText
                alertMessageTextNew =
                    httpErrorMessageScreen httpError
                        ++ " (while attempting to send Like to server)"
            in
            ( { model
                | alertMessageText = alertMessageTextNew
                , awaitingServerResponse = awaitingServerResponseInit
                , processingLike = processingLikeInit
                , songLiking = songLikingInit
              }
            , Cmd.none
            )

        LikeResponse (Ok appendLikeJson) ->
            likeResponse model appendLikeJson

        PageMorphHand ->
            --(alertMessage, awaitingServer, commentArea)
            case stateVector of
                _ ->
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
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , pageIsExpanded = pageIsExpandedNew
                      }
                    , focusInputPossibly model
                    )

        SongBuyAnchorProcessHand ->
            --(alertMessage, awaitingServer, commentArea)
            case stateVector of
                ( _, _, _ ) ->
                    ( model
                    , focusInputPossibly model
                    )

        SongForgetHand songRememberedIndex ->
            --(alertMessage, awaitingServer, commentArea)
            case stateVector of
                ( _, _, _ ) ->
                    let
                        songsRememberedWithoutOne : SongsRemembered
                        songsRememberedWithoutOne =
                            List.take songRememberedIndex model.songsRemembered
                                ++ List.drop (songRememberedIndex + 1) model.songsRemembered
                    in
                    if likingOrCommenting model then
                        --if String.isEmpty model.alertMessageText then
                        if model.processingComment then
                            ( { model
                                | alertMessageText = alertMessageTextInit
                                , awaitingServerResponse = awaitingServerResponseInit
                              }
                            , focusInputPossibly model
                            )
                        else if model.songCommentingIndex == Just songRememberedIndex then
                            ( { model
                                | alertMessageText = alertMessageTextInit
                                , awaitingServerResponse = awaitingServerResponseInit
                                , songCommentingIndex = songCommentingIndexInit
                              }
                            , Cmd.none
                            )
                        else
                            ( { model
                                | alertMessageText = alertMessageTextInit
                                , awaitingServerResponse = awaitingServerResponseInit
                                , songCommentingIndex = songCommentingIndexInit
                              }
                            , msg2Cmd (succeed (SongForgetHand songRememberedIndex))
                            )
                    else
                        ( { model
                            | songsRemembered = songsRememberedWithoutOne
                          }
                        , focusSet "refresh"
                        )

        SongRememberHand songLatestFewIndex ->
            --(alertMessage, awaitingServer, commentArea)
            case stateVector of
                ( _, _, _ ) ->
                    let
                        songClean : SongRemembered -> SongRemembered
                        songClean song =
                            { song | likedOrCommented = False }

                        songLatestFewSelected : Maybe SongLatestFew
                        songLatestFewSelected =
                            List.head (List.drop songLatestFewIndex model.songsLatestFew)
                    in
                    case songLatestFewSelected of
                        Nothing ->
                            ( model
                            , focusInputPossibly model
                            )

                        Just songLatestFewSelected ->
                            let
                                songDiffers : SongRemembered -> Bool
                                songDiffers songRemembered =
                                    songLatestFewSelected /= songRemembered2SongBasic songRemembered

                                songsDifferent : SongsRemembered
                                songsDifferent =
                                    List.filter songDiffers model.songsRemembered

                                songsRememberedNew : SongsRemembered
                                songsRememberedNew =
                                    if
                                        List.member
                                            songLatestFewSelected
                                            (List.map songRemembered2SongBasic model.songsRemembered)
                                    then
                                        model.songsRemembered
                                    else
                                        songsDifferent
                                            ++ [ songBasic2SongRemembered songLatestFewSelected ]
                            in
                            if likingOrCommenting model then
                                if model.processingComment then
                                    ( { model
                                        | alertMessageText = alertMessageTextInit
                                        , awaitingServerResponse = awaitingServerResponseInit
                                      }
                                    , focusInputPossibly model
                                    )
                                else
                                    ( { model
                                        | alertMessageText = alertMessageTextInit
                                        , awaitingServerResponse = awaitingServerResponseInit
                                        , songCommentingIndex = songCommentingIndexInit
                                      }
                                    , Cmd.none
                                    )
                            else
                                ( { model
                                    | songsRemembered = songsRememberedNew
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
            --(alertMessage, awaitingServer, commentArea)
            case stateVector of
                ( _, True, _ ) ->
                    ( model
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , awaitingServerResponse = awaitingServerResponseInit
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
