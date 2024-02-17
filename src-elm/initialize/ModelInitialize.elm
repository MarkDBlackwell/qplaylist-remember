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
import RequestUpdateType
    exposing
        ( AwaitingServerResponse
        )
import SongInitialize
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
        Alert.messageTextInit
        awaitingServerResponseInit
        commentTextInit
        pageIsExpandedInit
        flags.showCommentButtons
        SongInitialize.songCommentingOnNowMaybeInit
        SongInitialize.songLikingNowMaybeInit
        SongInitialize.songsRecentInit
        flags.songsRemembered
        UserIdentifier.userIdentifierInit
    , UserIdentifier.cmdGenerateUserIdentifier
    )


pageIsExpandedInit : PageIsExpanded
pageIsExpandedInit =
    False
