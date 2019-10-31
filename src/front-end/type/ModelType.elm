{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
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
