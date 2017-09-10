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
        , likeOrCommentResponse
        , likingOrCommenting
        )

import MessageDetails exposing (Msg(LogResponseOk))
import ModelDetails
    exposing
        ( Model
        , SongRemembered
        , SongsRemembered
        )
import ModelDetailsUpdate exposing (SongRememberedIndex)
import ModelInitialize
    exposing
        ( alertMessageInit
        , awaitingServerResponseInit
        , likeOrCommentTextInit
        , processingCommentInit
        , processingLikeInit
        , songRememberedCommentingIndexInit
        )
import Task exposing (succeed)
import UpdateUtilities
    exposing
        ( focusSet
        , logResponseOk
        , msg2Cmd
        )


-- UPDATE


focusInputPossibly : Model -> Cmd Msg
focusInputPossibly model =
    if likingOrCommenting model then
        focusSet "input"
    else
        Cmd.none


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
        | alertMessage = alertMessageInit
        , awaitingServerResponse = awaitingServerResponseInit
        , likeOrCommentText = likeOrCommentTextInit
        , processingComment = processingCommentInit
        , processingLike = processingLikeInit
        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
        , songsRemembered = songsRememberedNew
      }
    , msg2Cmd (succeed (LogResponseOk appendLikeOrCommentJson))
    )


likingOrCommenting : Model -> Bool
likingOrCommenting model =
    model.songRememberedCommentingIndex /= songRememberedCommentingIndexInit
