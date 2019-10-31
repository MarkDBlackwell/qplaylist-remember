{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ModelInitialize
    exposing
        ( awaitingServerResponseInit
        , commentTextInit
        , init
        )

import Alert
    exposing
        ( alertMessageTextInit
        )
import ElmCycle
    exposing
        ( ElmCycle
        )
import ModelType
    exposing
        ( CommentText
        , Flags
        , Model
        , PageIsExpanded
        )
import SongInitialize
    exposing
        ( songCommentingMaybeInit
        , songLikingMaybeInit
        , songsRecentInit
        )
import UpdateRequestType
    exposing
        ( AwaitingServerResponse
        )
import UserIdentifier
    exposing
        ( generateUserIdentifier
        , userIdentifierInit
        )


-- MODEL


awaitingServerResponseInit : AwaitingServerResponse
awaitingServerResponseInit =
    False


commentTextInit : CommentText
commentTextInit =
    ""


init : Flags -> ElmCycle
init flags =
    ( Model
        alertMessageTextInit
        awaitingServerResponseInit
        commentTextInit
        pageIsExpandedInit
        flags.showCommentButtons
        songCommentingMaybeInit
        songLikingMaybeInit
        songsRecentInit
        flags.songsRemembered
        userIdentifierInit
    , generateUserIdentifier
    )


pageIsExpandedInit : PageIsExpanded
pageIsExpandedInit =
    False
