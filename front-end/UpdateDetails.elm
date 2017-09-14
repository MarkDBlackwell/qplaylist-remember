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


module UpdateDetails
    exposing
        ( focusInputPossibly
        , likeOrCommentRequestUriText
        , likeOrCommentResponse
        , likeResponse
        , likingOrCommenting
        )

import MessageDetails exposing (Msg(HttpResponseTextLog))
import ModelDetails
    exposing
        ( ClosedOpen
            ( Closed
            , Open
            )
        , CommentAreaClosedOpen
        , Model
        , SongRemembered
        , SongRememberedCommentingIndex
        , SongsRemembered
        , songRemembered2LatestFew
        )
import ModelDetailsUpdate
    exposing
        ( AlertMessageClosedOpen
        , SongRememberedIndex
        , UriText
        )
import ModelInitialize
    exposing
        ( alertMessageTextInit
        , awaitingServerResponseInit
        , commentTextInit
        , processingCommentInit
        , processingLikeInit
        , songRememberedCommentingIndexInit
        , songRememberedLikingInit
        )
import Task exposing (succeed)
import UpdateUtilities
    exposing
        ( focusSet
        , msg2Cmd
        )
import ViewUtilities exposing (relative)


-- UPDATE


focusInputPossibly : Model -> Cmd Msg
focusInputPossibly model =
    if likingOrCommenting model then
        focusSet "input"
    else
        Cmd.none


likeOrCommentRequestUriText : Model -> String -> UriText
likeOrCommentRequestUriText model likeOrCommentText =
    let
        artistTimeTitle : UriText
        artistTimeTitle =
            case songRememberedCommentingIndex of
                Nothing ->
                    ""

                Just _ ->
                    case songSelected of
                        Nothing ->
                            ""

                        Just songSelected ->
                            songSelected.time
                                ++ " "
                                ++ songSelected.artist
                                ++ ": "
                                ++ songSelected.title

        basename : UriText
        basename =
            "append.php"

        songRememberedCommentingIndex : SongRememberedCommentingIndex
        songRememberedCommentingIndex =
            model.songRememberedCommentingIndex

        songSelected : Maybe SongRemembered
        songSelected =
            case songRememberedCommentingIndex of
                Nothing ->
                    Nothing

                Just index ->
                    List.head (List.drop index model.songsRemembered)

        timeStamp : UriText
        timeStamp =
            case songRememberedCommentingIndex of
                Nothing ->
                    ""

                Just _ ->
                    case songSelected of
                        Nothing ->
                            ""

                        Just song ->
                            song.timeStamp
    in
    relative
        [ basename ]
        [ ( "timestamp", timeStamp )
        , ( "song", artistTimeTitle )
        , ( "comment", likeOrCommentText )
        ]


likeOrCommentResponse : Model -> String -> ( Model, Cmd Msg )
likeOrCommentResponse model appendLikeOrCommentJson =
    let
        likedOrCommentedShow : SongRememberedIndex -> SongRemembered -> SongRemembered
        likedOrCommentedShow index song =
            if model.songRememberedCommentingIndex == Just index then
                { song
                    | likedOrCommented = True
                }
            else
                song

        songsRememberedNew : SongsRemembered
        songsRememberedNew =
            List.indexedMap likedOrCommentedShow model.songsRemembered
    in
    ( { model
        | alertMessageText = alertMessageTextInit
        , awaitingServerResponse = awaitingServerResponseInit
        , commentText = commentTextInit
        , processingComment = processingCommentInit
        , processingLike = processingLikeInit
        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
        , songsRemembered = songsRememberedNew
      }
    , msg2Cmd (succeed (HttpResponseTextLog appendLikeOrCommentJson))
    )


likeResponse : Model -> String -> ( Model, Cmd Msg )
likeResponse model appendLikeJson =
    let
        likedShow : SongRemembered -> SongRemembered
        likedShow song =
            case model.songRememberedLiking of
                Nothing ->
                    song

                Just songRememberedLiking ->
                    if songRemembered2LatestFew song /= songRememberedLiking then
                        song
                    else
                        { song
                            | likedOrCommented = True
                        }

        songsRememberedNew : SongsRemembered
        songsRememberedNew =
            List.map likedShow model.songsRemembered
    in
    ( { model
        | alertMessageText = alertMessageTextInit
        , awaitingServerResponse = awaitingServerResponseInit
        , processingLike = processingLikeInit
        , songRememberedLiking = songRememberedLikingInit
        , songsRemembered = songsRememberedNew
      }
    , msg2Cmd (succeed (HttpResponseTextLog appendLikeJson))
    )


likingOrCommenting : Model -> Bool
likingOrCommenting model =
    --model.songRememberedCommentingIndex /= songRememberedCommentingIndexInit
    model.processingComment
        || model.processingLike
