{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module RequestUpdateType exposing
    ( Action(..)
    , ActionLikeOrComment(..)
    , AwaitingServerResponse
    , HttpResponseText
    , LikeOrCommentResponseText
    , LikeOrCommentText
    , QueryPair
    , QueryPairs
    , UrlBeforeQueryList
    , UrlText
    )

import Url.Builder



-- MODEL


type alias AwaitingServerResponse =
    Bool



-- UPDATE


type Action
    = ActionDecoding
    | ActionRequest
    | ActionResponse


type ActionLikeOrComment
    = Comment
    | Like


type alias HttpResponseText =
    String


type alias LikeOrCommentResponseText =
    String


type alias LikeOrCommentText =
    String


type alias QueryPair =
    Url.Builder.QueryParameter


type alias QueryPairs =
    List QueryPair


type alias UrlText =
    String


type alias UrlBeforeQueryList =
    --See:
    --  http://github.com/elm/url
    --  http://tools.ietf.org/html/rfc3986
    --When joined, then comprises a URL's scheme, authority, and path:
    List UrlText
