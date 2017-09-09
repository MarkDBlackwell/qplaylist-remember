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


module Update exposing (..)

import Debug exposing (log)
import DecodeLatestFew exposing (decodeSongsLatestFew)
import Dom
    exposing
        ( Id
        , focus
        )
import Http
    exposing
        ( Error
        , Request
        , getString
        , send
        )
import MessageDetails exposing (..)
import ModelDetails exposing (..)
import ModelDetailsUpdate
    exposing
        ( HttpErrorMessageText
        , HttpRequestText
        , SongRememberedIndex
        , UriText
        )
import ModelInitialize exposing (..)
import Task
    exposing
        ( attempt
        , succeed
        )
import Utilities exposing (msg2Cmd)
import View
    exposing
        ( likeOrCommentRequestUriText
        , relative
        )


-- UPDATE


focusSet : Id -> Cmd Msg
focusSet id =
    msg2Cmd (succeed (FocusSet id))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        alertMessageSuffix : AlertMessage -> AlertMessage
        alertMessageSuffix thing =
            " (while attempting to send "
                ++ thing
                ++ " to server)"

        focusInputPossibly : Cmd Msg
        focusInputPossibly =
            if model.songRememberedCommentingIndex == songRememberedCommentingIndexInit then
                Cmd.none
            else
                focusSet "input"

        httpErrorMessageText : Error -> HttpErrorMessageText
        httpErrorMessageText httpError =
            let
                prefix : HttpErrorMessageText
                prefix =
                    "HttpError"
            in
            case httpError of
                Http.BadPayload debuggingText httpResponseText ->
                    log (prefix ++ ": BadPayload") debuggingText

                Http.BadStatus httpResponseText ->
                    log prefix "BadStatus"

                Http.BadUrl uriText ->
                    log (prefix ++ ": BadUrl") uriText

                Http.NetworkError ->
                    log prefix "NetworkError"

                Http.Timeout ->
                    log prefix "Timeout"

        likeOrCommentResponse : String -> ( Model, Cmd Msg )
        likeOrCommentResponse appendLikeOrCommentJson =
            let
                --Keep for console logging:
                a : String
                a =
                    logResponseOk appendLikeOrCommentJson

                sharedShow : SongsRemembered
                sharedShow =
                    List.indexedMap sharedShowSong model.songsRemembered

                sharedShowSong : SongRememberedIndex -> SongRemembered -> SongRemembered
                sharedShowSong index song =
                    if Just index == model.songRememberedCommentingIndex then
                        { song
                            | likedOrCommented = True
                        }
                    else
                        song
            in
            ( { model
                | alertMessage = alertMessageInit
                , awaitingServerResponse = awaitingServerResponseInit
                , likeOrCommentText = likeOrCommentTextInit
                , processingComment = processingCommentInit
                , processingLike = processingLikeInit
                , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                , songsRemembered = sharedShow
              }
            , Cmd.none
            )

        likingOrCommenting : Bool
        likingOrCommenting =
            model.songRememberedCommentingIndex /= songRememberedCommentingIndexInit

        logResponseOk : String -> String
        logResponseOk string =
            --log "Ok response" string
            log "Response" "Ok"
    in
    case msg of
        BuySongAnchorProcess ->
            ( model
            , focusInputPossibly
            )

        CommentInputCancel ->
            ( { model
                | alertMessage = alertMessageInit
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
                    | alertMessage = alertMessageInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly
                )
            else
                ( { model
                    | awaitingServerResponse = True
                  }
                , Cmd.batch [ focusInputPossibly, commentRequest ]
                )

        CommentInputSetUp songRememberedIndex ->
            if likingOrCommenting then
                ( { model
                    | alertMessage = alertMessageInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly
                )
            else
                case model.songRememberedCommentingIndex of
                    Just _ ->
                        ( model
                          --, focusSet "refresh"
                        , focusInputPossibly
                        )

                    songRememberedCommentingIndexInit ->
                        ( { model
                            | processingComment = True
                            , songRememberedCommentingIndex = Just songRememberedIndex
                          }
                          --'focusInputPossibly' doesn't work, here:
                        , focusSet "input"
                        )

        CommentInputTextChangeCapture text ->
            ( { model
                | alertMessage = alertMessageInit
                , awaitingServerResponse = awaitingServerResponseInit
                , likeOrCommentText = text
              }
            , Cmd.none
            )

        CommentResponse (Err httpError) ->
            let
                alertMessageNew : AlertMessage
                alertMessageNew =
                    httpErrorMessageText httpError ++ alertMessageSuffix "comment"
            in
            ( { model
                | alertMessage = alertMessageNew
              }
            , focusInputPossibly
            )

        CommentResponse (Ok appendCommentJson) ->
            likeOrCommentResponse appendCommentJson

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
            if likingOrCommenting then
                if model.processingComment then
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                      }
                    , focusInputPossibly
                    )
                else
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                      }
                    , msg2Cmd (succeed (LikeButtonProcess songRememberedIndex))
                    )
            else
                case model.songRememberedCommentingIndex of
                    Just _ ->
                        ( { model
                            | alertMessage = alertMessageInit
                            , awaitingServerResponse = awaitingServerResponseInit
                          }
                        , focusInputPossibly
                        )

                    songRememberedCommentingIndexInit ->
                        ( { model
                            | alertMessage = alertMessageInit
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
                alertMessageNew : AlertMessage
                alertMessageNew =
                    httpErrorMessageText httpError ++ alertMessageSuffix "Like"
            in
            ( { model
                | alertMessage = alertMessageNew
                , awaitingServerResponse = awaitingServerResponseInit
                , likeOrCommentText = likeOrCommentTextInit
                , processingComment = processingCommentInit
                , processingLike = processingLikeInit
                , songRememberedCommentingIndex = songRememberedCommentingIndexInit
              }
            , Cmd.none
            )

        LikeResponse (Ok appendLikeJson) ->
            likeOrCommentResponse appendLikeJson

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
            if likingOrCommenting then
                ( { model
                    | alertMessage = alertMessageInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly
                )
            else
                ( { model
                    | alertMessage = alertMessageInit
                    , pageIsExpanded = pageIsExpandedNew
                  }
                , focusInputPossibly
                )

        SongForget songRememberedIndex ->
            let
                songsRememberedWithoutOne : SongsRemembered
                songsRememberedWithoutOne =
                    List.take songRememberedIndex model.songsRemembered
                        ++ List.drop (songRememberedIndex + 1) model.songsRemembered
            in
            if likingOrCommenting then
                --if String.isEmpty model.alertMessage then
                if model.processingComment then
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                      }
                    , focusInputPossibly
                    )
                else if model.songRememberedCommentingIndex == Just songRememberedIndex then
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                      }
                    , Cmd.none
                    )
                else
                    ( { model
                        | alertMessage = alertMessageInit
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

                songDiffers : SongRemembered -> Bool
                songDiffers song =
                    case songSelected of
                        Nothing ->
                            True

                        Just songSelected ->
                            songClean (songLatestFew2Remembered songSelected) /= songClean song

                songSelected : Maybe SongLatestFew
                songSelected =
                    List.head (List.drop songLatestFewIndex model.songsLatestFew)

                songsDifferent : SongsRemembered
                songsDifferent =
                    if Nothing == songSelected then
                        model.songsRemembered
                    else
                        List.filter songDiffers model.songsRemembered

                songsRememberedCleaned : SongsRemembered
                songsRememberedCleaned =
                    List.map songClean model.songsRemembered

                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    case songSelected of
                        Nothing ->
                            model.songsRemembered

                        Just songSelected ->
                            if
                                List.member
                                    (songClean (songLatestFew2Remembered songSelected))
                                    songsRememberedCleaned
                            then
                                model.songsRemembered
                            else
                                songsDifferent
                                    ++ [ songLatestFew2Remembered songSelected ]
            in
            if likingOrCommenting then
                if model.processingComment then
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                      }
                    , focusInputPossibly
                    )
                else
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                      }
                    , Cmd.none
                    )
            else
                ( { model
                    | songsRemembered = songsRememberedNew
                  }
                , focusInputPossibly
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
            if likingOrCommenting then
                ( { model
                    | alertMessage = alertMessageInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly
                )
            else
                ( model
                , Cmd.batch [ focusInputPossibly, songsLatestFewRequest ]
                )

        SongsLatestFewResponse (Err httpError) ->
            let
                alertMessageNew : AlertMessage
                alertMessageNew =
                    httpErrorMessageText httpError ++ suffix

                suffix : HttpErrorMessageText
                suffix =
                    " (while attempting to access the latest few songs)"
            in
            ( { model
                | alertMessage = alertMessageNew
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
                | alertMessage = alertMessageInit
                , songsLatestFew = songsLatestFewNew
              }
            , Cmd.none
            )
