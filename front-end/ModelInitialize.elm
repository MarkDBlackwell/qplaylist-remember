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
        , CommentText
        , LikedOrCommented
        , Model
        , Optional
            ( Closed
            , Open
            )
        , PageIsExpanded
        , SongCommenting
        , SongLiking
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


commentTextInit : CommentText
commentTextInit =
    ""


init : ( Model, Cmd msg )
init =
    ( Model
        alertMessageTextInit
        awaitingServerResponseInit
        commentTextInit
        pageIsExpandedInit
        songCommentingInit
        songLikingInit
        songsLatestFewInit
        songsRememberedInit
    , Cmd.none
    )


likedOrCommentedInit : LikedOrCommented
likedOrCommentedInit =
    False


pageIsExpandedInit : PageIsExpanded
pageIsExpandedInit =
    False


songCommentingInit : SongCommenting
songCommentingInit =
    Nothing


songLikingInit : SongLiking
songLikingInit =
    Nothing


songsLatestFewInit : SongsLatestFew
songsLatestFewInit =
    []


songsRememberedInit : SongsRemembered
songsRememberedInit =
    []
