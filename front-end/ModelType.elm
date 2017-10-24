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


module ModelType
    exposing
        ( CommentText
        , Flags
        , Model
        , Optional
            ( Closed
            , Open
            )
        , PageIsExpanded
        , ShowCommentButtons
        )

import AlertType
    exposing
        ( AlertMessageTextMaybe
        )
import SongType
    exposing
        ( SongRememberedMaybe
        , SongsRecent
        , SongsRemembered
        )
import UpdateRequestType
    exposing
        ( AwaitingServerResponse
        )
import UserIdentifierType
    exposing
        ( UserIdentifier
        )


-- MODEL


type alias CommentText =
    String


type alias Flags =
    { showCommentButtons : ShowCommentButtons
    , songsRemembered : SongsRemembered
    }


type alias Model =
    { alertMessageText : AlertMessageTextMaybe
    , awaitingServerResponse : AwaitingServerResponse
    , commentText : CommentText
    , pageIsExpanded : PageIsExpanded
    , showCommentButtons : ShowCommentButtons
    , songCommentingMaybe : SongRememberedMaybe
    , songLikingMaybe : SongRememberedMaybe
    , songsRecent : SongsRecent
    , songsRemembered : SongsRemembered
    , userIdentifier : UserIdentifier
    }


type alias PageIsExpanded =
    Bool


type alias ShowCommentButtons =
    Bool



-- UPDATE


type Optional
    = Closed
    | Open
