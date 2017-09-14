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


module ModelInitialize
    exposing
        ( alertMessageTextInit
        , awaitingServerResponseInit
        , commentTextInit
        , init
        , pageIsExpandedInit
        , processingCommentInit
        , processingLikeInit
        , songRememberedCommentingIndexInit
        , songRememberedLikingInit
        , songsLatestFewInit
        , songsRememberedInit
        )

import ModelDetails
    exposing
        ( AlertMessageText
        , AwaitingServerResponse
        , ClosedOpen
            ( Closed
            , Open
            )
        , CommentAreaClosedOpen
        , CommentText
        , Model
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


commentAreaClosedOpenInit : CommentAreaClosedOpen
commentAreaClosedOpenInit =
    Closed


init : ( Model, Cmd msg )
init =
    ( Model
        alertMessageTextInit
        awaitingServerResponseInit
        commentAreaClosedOpenInit
        commentTextInit
        pageIsExpandedInit
        processingCommentInit
        processingLikeInit
        songRememberedCommentingInit
        songRememberedCommentingIndexInit
        songRememberedLikingInit
        songsLatestFewInit
        songsRememberedInit
    , Cmd.none
    )


commentTextInit : CommentText
commentTextInit =
    ""


pageIsExpandedInit : PageIsExpanded
pageIsExpandedInit =
    False


processingCommentInit : ProcessingComment
processingCommentInit =
    False


processingLikeInit : ProcessingLike
processingLikeInit =
    False


songRememberedCommentingIndexInit : SongRememberedCommentingIndex
songRememberedCommentingIndexInit =
    Nothing


songRememberedCommentingInit : SongRememberedCommenting
songRememberedCommentingInit =
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
