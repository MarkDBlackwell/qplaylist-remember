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
        ( actionsDelayInit
        , alertMessageTextInit
        , awaitingServerResponseInit
        , init
        , likeOrCommentTextInit
        , pageIsExpandedInit
        , processingCommentInit
        , processingLikeInit
        , songRememberedCommentingIndexInit
        , songsLatestFewInit
        , songsRememberedInit
        )

import ModelDetails
    exposing
        ( ActionsDelay
        , AlertMessageText
        , AwaitingServerResponse
        , ClosedOpen
            ( Closed
            , Open
            )
        , CommentAreaClosedOpen
        , LikeOrCommentText
        , Model
        , PageIsExpanded
        , ProcessingComment
        , ProcessingLike
        , SongRememberedCommentingIndex
        , SongsLatestFew
        , SongsRemembered
        )


-- MODEL


actionsDelayInit : ActionsDelay
actionsDelayInit =
    False


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
        actionsDelayInit
        alertMessageTextInit
        awaitingServerResponseInit
        commentAreaClosedOpenInit
        likeOrCommentTextInit
        pageIsExpandedInit
        processingCommentInit
        processingLikeInit
        songRememberedCommentingIndexInit
        songsLatestFewInit
        songsRememberedInit
    , Cmd.none
    )


likeOrCommentTextInit : LikeOrCommentText
likeOrCommentTextInit =
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


songsLatestFewInit : SongsLatestFew
songsLatestFewInit =
    []


songsRememberedInit : SongsRemembered
songsRememberedInit =
    []
