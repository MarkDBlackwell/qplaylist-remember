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
import DecodeLatestFew exposing (decodeSongsLatestFew)
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
        , LikeOrCommentText
        , Model
        , PageIsExpanded
        , SongLatestFew
        , SongRemembered
        , SongsLatestFew
        , SongsRemembered
        , songLatestFew2Remembered
        , songRemembered2LatestFew
        )
import ModelDetailsUpdate
    exposing
        ( HttpErrorMessageText
        , HttpRequestText
        , UriText
        )
import ModelInitialize
    exposing
        ( alertMessageTextInit
        , awaitingServerResponseInit
        , likeOrCommentTextInit
        , processingCommentInit
        , processingLikeInit
        , songRememberedCommentingIndexInit
        )
import Task
    exposing
        ( attempt
        , succeed
        )
import UpdateDetails
    exposing
        ( focusInputPossibly
        , likeOrCommentResponse
        , likingOrCommenting
        )
import UpdateUtilities
    exposing
        ( focusSet
        , httpErrorMessageLogging
        , httpErrorMessageScreen
        , msg2Cmd
        )
import View exposing (likeOrCommentRequestUriText)
import ViewUtilities exposing (relative)


-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        BuySongAnchorProcess ->
            ( model
            , focusInputPossibly model
            )

        CommentInputCancel ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , likeOrCommentText = likeOrCommentTextInit
                , processingComment = processingCommentInit
                , songRememberedCommentingIndex = songRememberedCommentingIndexInit
              }
            , Cmd.none
            )

        CommentInputOk ->
            let
                commentRequest : Cmd Msg
                commentRequest =
                    send CommentResponse (getString (log "Request" (likeOrCommentRequestUriText model)))
            in
            if String.isEmpty model.likeOrCommentText then
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
                , Cmd.batch [ focusInputPossibly model, commentRequest ]
                )

        CommentInputSetUp songRememberedIndex ->
            if likingOrCommenting model then
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly model
                )
            else
                case model.songRememberedCommentingIndex of
                    Just _ ->
                        ( model
                          --, focusSet "refresh"
                        , focusInputPossibly model
                        )

                    Nothing ->
                        ( { model
                            | processingComment = True
                            , songRememberedCommentingIndex = Just songRememberedIndex
                          }
                          --'focusInputPossibly' doesn't work, here:
                        , focusSet "input"
                        )

        CommentInputTextChangeCapture text ->
            ( { model
                | alertMessageText = alertMessageTextInit
                , awaitingServerResponse = awaitingServerResponseInit
                , likeOrCommentText = text
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
              }
            , Cmd.batch
                [ msg2Cmd (succeed (ResponseLog (httpErrorMessageLogging httpError)))
                , focusInputPossibly model
                ]
            )

        CommentResponse (Ok appendCommentJson) ->
            likeOrCommentResponse model appendCommentJson

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

        LikeButtonProcess songRememberedIndex ->
            let
                likeText : LikeOrCommentText
                likeText =
                    "Loved it!"
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
                        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                      }
                    , msg2Cmd (succeed (LikeButtonProcess songRememberedIndex))
                    )
            else
                case model.songRememberedCommentingIndex of
                    Just _ ->
                        ( { model
                            | alertMessageText = alertMessageTextInit
                            , awaitingServerResponse = awaitingServerResponseInit
                          }
                        , focusInputPossibly model
                        )

                    Nothing ->
                        ( { model
                            | alertMessageText = alertMessageTextInit
                            , awaitingServerResponse = True
                            , likeOrCommentText = likeText
                            , processingLike = True
                            , songRememberedCommentingIndex = Just songRememberedIndex
                          }
                        , msg2Cmd (succeed LikeRequest)
                        )

        LikeRequest ->
            ( model
            , send LikeResponse (getString (log "Request" (likeOrCommentRequestUriText model)))
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
                , likeOrCommentText = likeOrCommentTextInit
                , processingComment = processingCommentInit
                , processingLike = processingLikeInit
                , songRememberedCommentingIndex = songRememberedCommentingIndexInit
              }
            , Cmd.none
            )

        LikeResponse (Ok appendLikeJson) ->
            likeOrCommentResponse model appendLikeJson

        PageMorph ->
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
            if likingOrCommenting model then
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly model
                )
            else
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , pageIsExpanded = pageIsExpandedNew
                  }
                , focusInputPossibly model
                )

        ResponseLog httpResponseText ->
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

        SongForget songRememberedIndex ->
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
                else if model.songRememberedCommentingIndex == Just songRememberedIndex then
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , awaitingServerResponse = awaitingServerResponseInit
                        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                      }
                    , Cmd.none
                    )
                else
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , awaitingServerResponse = awaitingServerResponseInit
                        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                      }
                    , msg2Cmd (succeed (SongForget songRememberedIndex))
                    )
            else
                ( { model
                    | songsRemembered = songsRememberedWithoutOne
                  }
                , focusSet "refresh"
                )

        SongRemember songLatestFewIndex ->
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
                            songLatestFewSelected /= songRemembered2LatestFew songRemembered

                        songsDifferent : SongsRemembered
                        songsDifferent =
                            List.filter songDiffers model.songsRemembered

                        songsRememberedNew : SongsRemembered
                        songsRememberedNew =
                            if
                                List.member
                                    songLatestFewSelected
                                    (List.map songRemembered2LatestFew model.songsRemembered)
                            then
                                model.songsRemembered
                            else
                                songsDifferent
                                    ++ [ songLatestFew2Remembered songLatestFewSelected ]
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
                                , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                              }
                            , Cmd.none
                            )
                    else
                        ( { model
                            | songsRemembered = songsRememberedNew
                          }
                        , focusInputPossibly model
                        )

        SongsLatestFewRefresh ->
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
            if likingOrCommenting model then
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly model
                )
            else
                ( model
                , Cmd.batch [ focusInputPossibly model, songsLatestFewRequest ]
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
              }
            , Cmd.none
            )

        SongsLatestFewResponse (Ok jsonRawText) ->
            let
                songsLatestFewNew : SongsLatestFew
                songsLatestFewNew =
                    decodeSongsLatestFew jsonRawText
            in
            ( { model
                | alertMessageText = alertMessageTextInit
                , songsLatestFew = songsLatestFewNew
              }
            , Cmd.none
            )
