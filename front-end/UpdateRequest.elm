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
        , songsLatestRefreshHand
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
            , SongsLatestResponse
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
        ( songLikingOrCommentingMaybe
        , songsRememberedUpdateTimestamp
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
import UpdateLog
    exposing
        ( logRequest
        )
import UpdateRequestHelper
    exposing
        ( likeOrCommentRequestUriText
        , relative
        )
import UpdateRequestType
    exposing
        ( HttpRequestText
        , UriText
        )
import UpdateStateVector
    exposing
        ( stateVector
        )


-- UPDATE


commentSendHand : Model -> ElmCycle
commentSendHand model =
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
            songLikingOrCommentingMaybe songsRememberedNew songsRememberedIndex

        songsRememberedNew : SongsRemembered
        songsRememberedNew =
            songsRememberedUpdateTimestamp
                model.songsLatest
                model.songsRemembered
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


songsLatestRefreshHand : Model -> ElmCycle
songsLatestRefreshHand model =
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
                , songsLatestRequest
                , focusInputPossibly model
                ]
            )
