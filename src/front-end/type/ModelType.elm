{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ModelType exposing
    ( CommentAreaOptional(..)
    , CommentText
    , Flags
    , Model
    , PageIsExpanded
    , ShowCommentButtons
    )

import AlertType
    exposing
        ( AlertMessageTextMaybe
        )
import RequestUpdateType
    exposing
        ( AwaitingServerResponse
        )
import SongType
    exposing
        ( SongRememberedMaybe
        , SongsRecent
        , SongsRemembered
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


type CommentAreaOptional
    = CommentAreaClosed
    | CommentAreaOpen
