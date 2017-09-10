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


module UpdateDetails exposing (..)

import Debug exposing (log)
import MessageDetails exposing (Msg)
import ModelDetails
    exposing
        ( AlertMessage
        , Model
        , SongRemembered
        , SongsRemembered
        )
import ModelDetailsUpdate exposing (SongRememberedIndex)
import ModelInitialize exposing (..)
import UpdateUtilities exposing (focusSet)


-- UPDATE


alertMessageSuffix : AlertMessage -> AlertMessage
alertMessageSuffix thing =
    " (while attempting to send "
        ++ thing
        ++ " to server)"


focusInputPossibly : Model -> Cmd Msg
focusInputPossibly model =
    if model.songRememberedCommentingIndex == songRememberedCommentingIndexInit then
        Cmd.none
    else
        focusSet "input"


likeOrCommentResponse : Model -> String -> ( Model, Cmd Msg )
likeOrCommentResponse model appendLikeOrCommentJson =
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


likingOrCommenting : Model -> Bool
likingOrCommenting model =
    model.songRememberedCommentingIndex /= songRememberedCommentingIndexInit


logResponseOk : String -> String
logResponseOk string =
    --log "Ok response" string
    log "Response" "Ok"
