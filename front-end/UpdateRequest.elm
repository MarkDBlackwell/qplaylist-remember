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
import Http
    exposing
        ( Request
        , getString
        , send
        )
import MessageType
    exposing
        ( Msg
            ( CommentResponse
            , LikeResponse
            , SongsLatestResponse
            )
        )
import ModelType
    exposing
        ( Model
        )
import Request
    exposing
        ( HttpRequestText
        , UriText
        , likeOrCommentRequestUriText
        , relative
        )
import Song
    exposing
        ( SongLikingOrCommentingMaybe
        , SongsRemembered
        , SongsRememberedIndex
        , songLikingOrCommentingMaybe
        , songsRememberedUpdateTimestamp
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        )
import UpdateLog
    exposing
        ( logMakeRequestAndFocus
        )
import UpdateStateVector
    exposing
        ( stateVector
        )


-- UPDATE


commentSendHand : Model -> ( Model, Cmd Msg )
commentSendHand model =
    let
        commentRequest : Cmd Msg
        commentRequest =
            send CommentResponse (getString commentRequestUriText)

        commentRequestUriText : UriText
        commentRequestUriText =
            likeOrCommentRequestUriText model.songCommentingMaybe model.userIdentifier model.commentText
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
                , logMakeRequestAndFocus model commentRequest "Request" commentRequestUriText
                )


likeButtonProcessHand : Model -> SongsRememberedIndex -> ( Model, Cmd Msg )
likeButtonProcessHand model songsRememberedIndex =
    let
        likeRequest : Cmd Msg
        likeRequest =
            send LikeResponse (getString likeRequestUriText)

        likeRequestUriText : UriText
        likeRequestUriText =
            likeOrCommentRequestUriText songLikingNew model.userIdentifier "Loved it!"

        songLikingNew : SongLikingOrCommentingMaybe
        songLikingNew =
            songLikingOrCommentingMaybe songsRememberedNew songsRememberedIndex

        songsRememberedNew : SongsRemembered
        songsRememberedNew =
            songsRememberedUpdateTimestamp model.songsLatest model.songsRemembered songsRememberedIndex
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
                , songLiking = songLikingNew
                , songsRemembered = songsRememberedNew
              }
            , logMakeRequestAndFocus model likeRequest "Request" likeRequestUriText
            )


songsLatestRefreshHand : Model -> ( Model, Cmd Msg )
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
            , logMakeRequestAndFocus model songsLatestRequest "Request" requestUriText
            )
