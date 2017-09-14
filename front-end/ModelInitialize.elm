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


module ModelInitialize exposing (..)

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
        , ProcessingComment
        , ProcessingLike
        , SongRememberedCommenting
        , SongRememberedCommentingIndex
        , SongRememberedLiking
        , SongsLatestFew
        , SongsRemembered
        )


-- MODEL


alertMessageTextInit : AlertMessageText
alertMessageTextInit =
    ""


awaitingServerResponseInit : AwaitingServerResponse
awaitingServerResponseInit =
    False


commentAreaOptionalInit : CommentAreaOptional
commentAreaOptionalInit =
    Closed


commentTextInit : CommentText
commentTextInit =
    ""


init : ( Model, Cmd msg )
init =
    ( Model
        alertMessageTextInit
        awaitingServerResponseInit
        commentAreaOptionalInit
        commentTextInit
        pageIsExpandedInit
        processingCommentInit
        processingLikeInit
        songCommentingInit
        songCommentingIndexInit
        songRememberedLikingInit
        songsLatestFewInit
        songsRememberedInit
    , Cmd.none
    )


pageIsExpandedInit : PageIsExpanded
pageIsExpandedInit =
    False


processingCommentInit : ProcessingComment
processingCommentInit =
    False


processingLikeInit : ProcessingLike
processingLikeInit =
    False


songCommentingIndexInit : SongRememberedCommentingIndex
songCommentingIndexInit =
    Nothing


songCommentingInit : SongRememberedCommenting
songCommentingInit =
    Nothing


songRememberedLikingInit : SongRememberedLiking
songRememberedLikingInit =
    Nothing


songsLatestFewInit : SongsLatestFew
songsLatestFewInit =
    []


songsRememberedInit : SongsRemembered
songsRememberedInit =
    []
