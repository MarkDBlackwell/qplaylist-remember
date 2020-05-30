{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ModelInitialize exposing
    ( awaitingServerResponseInit
    , commentTextInit
    , init
    )

import Alert
import ElmCycle
import ModelType
    exposing
        ( CommentText
        , Flags
        , Model
        , PageIsExpanded
        )
import SongInitialize
import UpdateRequestType
    exposing
        ( AwaitingServerResponse
        )
import UserIdentifier


awaitingServerResponseInit : AwaitingServerResponse
awaitingServerResponseInit =
    False


commentTextInit : CommentText
commentTextInit =
    ""



-- MODEL


init : Flags -> ElmCycle.ElmCycle
init flags =
    ( Model
        Alert.alertMessageTextInit
        awaitingServerResponseInit
        commentTextInit
        pageIsExpandedInit
        flags.showCommentButtons
        SongInitialize.songCommentingMaybeInit
        SongInitialize.songLikingMaybeInit
        SongInitialize.songsRecentInit
        flags.songsRemembered
        UserIdentifier.userIdentifierInit
    , UserIdentifier.generateUserIdentifier
    )


pageIsExpandedInit : PageIsExpanded
pageIsExpandedInit =
    False
