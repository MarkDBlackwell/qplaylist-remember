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


module UpdateRequest
    exposing
        ( commentSendHand
        , likeButtonProcessHand
        , songsRecentRefreshHand
        )

import Alert
    exposing
        ( alertMessageTextInit
        , alertMessageTextServerAwaiting
        )
import ElmCycle
    exposing
        ( ElmCycle
        , Msg
            ( CommentResponse
            , LikeResponse
            , SongsRecentResponse
            )
        )
import Http
    exposing
        ( Request
        , getString
        , send
        )
import ModelType
    exposing
        ( Model
        )
import Song
    exposing
        ( songsRememberedUpdateTimestampFromIndex
        )
import SongType
    exposing
        ( SongLikingOrCommentingMaybe
        , SongsRemembered
        , SongsRememberedIndex
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        )
import UpdateHelper
    exposing
        ( likeOrCommentRequestUriText
        , relative
        , stateVector
        )
import UpdateLog
    exposing
        ( logRequest
        )
import UpdateRequestType
    exposing
        ( HttpRequestText
        , UriText
        )
import Utilities
    exposing
        ( selectOneMaybe
        )


-- UPDATE


commentSendHand : Model -> ElmCycle
commentSendHand model =
    --(awaitingServer, commentArea)
    case stateVector model of
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
                let
                    commentRequest : Cmd Msg
                    commentRequest =
                        getString commentRequestUriText
                            |> send CommentResponse

                    commentRequestUriText : UriText
                    commentRequestUriText =
                        likeOrCommentRequestUriText
                            model.songCommentingMaybe
                            model.userIdentifier
                            model.commentText
                in
                ( { model
                    | alertMessageText = alertMessageTextInit
                    , awaitingServerResponse = True
                  }
                , Cmd.batch
                    [ logRequest commentRequestUriText
                    , commentRequest
                    , focusInputPossibly model
                    ]
                )


likeButtonProcessHand : Model -> SongsRememberedIndex -> ElmCycle
likeButtonProcessHand model songsRememberedIndex =
    let
        likeRequest : Cmd Msg
        likeRequest =
            getString likeRequestUriText
                |> send LikeResponse

        likeRequestUriText : UriText
        likeRequestUriText =
            likeOrCommentRequestUriText
                songLikingMaybeNew
                model.userIdentifier
                "Loved it!"

        songLikingMaybeNew : SongLikingOrCommentingMaybe
        songLikingMaybeNew =
            selectOneMaybe songsRememberedNew songsRememberedIndex

        songsRememberedNew : SongsRemembered
        songsRememberedNew =
            songsRememberedUpdateTimestampFromIndex
                model.songsRemembered
                model.songsRecent
                songsRememberedIndex
    in
    --(awaitingServer, commentArea)
    case stateVector model of
        ( True, _ ) ->
            ( { model
                | alertMessageText = alertMessageTextServerAwaiting
              }
            , focusInputPossibly model
            )

        _ ->
            case songLikingMaybeNew of
                Nothing ->
                    ( model
                    , focusInputPossibly model
                    )

                _ ->
                    ( { model
                        | alertMessageText = alertMessageTextInit
                        , awaitingServerResponse = True
                        , songLikingMaybe = songLikingMaybeNew
                        , songsRemembered = songsRememberedNew
                      }
                    , Cmd.batch
                        [ logRequest likeRequestUriText
                        , likeRequest
                        , focusInputPossibly model
                        ]
                    )


songsRecentRefreshHand : Model -> ElmCycle
songsRecentRefreshHand model =
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
                [ "../.."
                , subUri
                , basename
                ]
                []

        songsRecentRequest : Cmd Msg
        songsRecentRequest =
            let
                requestHttp : Request HttpRequestText
                requestHttp =
                    getString requestUriText
            in
            send SongsRecentResponse requestHttp
    in
    --(awaitingServer, commentArea)
    case stateVector model of
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
                [ logRequest requestUriText
                , songsRecentRequest
                , focusInputPossibly model
                ]
            )
