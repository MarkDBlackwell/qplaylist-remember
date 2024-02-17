{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UpdateHelper exposing
    ( actionLikeOrComment2String
    , commentAreaStateVector
    , elmCycleDefault
    , likeOrCommentRequestUrlText
    )

import ElmCycle
import FocusUpdate
import ModelType
    exposing
        ( CommentAreaOptional(..)
        , Model
        )
import RequestUpdateType
    exposing
        ( ActionLikeOrComment(..)
        , AwaitingServerResponse
        , LikeOrCommentText
        , QueryPairs
        , UrlBeforeQueryList
        , UrlText
        )
import SongInitialize
import SongType
    exposing
        ( SongRecentMaybe
        , SongRemembered
        , SongRememberedMaybe
        )
import Url.Builder
import UserIdentifierType
    exposing
        ( UserIdentifier
        )



-- UPDATE


actionLikeOrComment2String : ActionLikeOrComment -> String
actionLikeOrComment2String actionLikeOrComment =
    case actionLikeOrComment of
        Comment ->
            "Comment"

        Like ->
            "Like"


commentAreaStateVector : Model -> ( AwaitingServerResponse, CommentAreaOptional )
commentAreaStateVector model =
    let
        commentAreaOptional : CommentAreaOptional
        commentAreaOptional =
            case model.songCommentingOnNowMaybe of
                Nothing ->
                    CommentAreaClosed

                _ ->
                    CommentAreaOpen
    in
    ( model.awaitingServerResponse
    , commentAreaOptional
    )


elmCycleDefault : Model -> ElmCycle.ElmCycle
elmCycleDefault model =
    ( model
    , FocusUpdate.cmdFocusInputPossibly model
    )


likeOrCommentRequestUrlText : UserIdentifier -> SongRememberedMaybe -> UrlText -> LikeOrCommentText -> UrlText
likeOrCommentRequestUrlText userIdentifier songLikingOrCommentingOnNowMaybe commentCategory likeOrCommentText =
    let
        path : List String
        path =
            "append.json"
                |> List.singleton

        queryPairs : QueryPairs
        queryPairs =
            let
                songCategory : UrlText
                songCategory =
                    "s"

                songLikingOrCommenting : SongRemembered
                songLikingOrCommenting =
                    songLikingOrCommentingOnNowMaybe
                        |> Maybe.withDefault
                            SongInitialize.songLikingOrCommentingInit
            in
            [ Url.Builder.string "comment" likeOrCommentText
            , Url.Builder.string "comment_category" commentCategory
            , Url.Builder.string "song_artist" songLikingOrCommenting.artist
            , Url.Builder.string "song_category" songCategory
            , Url.Builder.string "song_time" songLikingOrCommenting.time
            , Url.Builder.string "song_title" songLikingOrCommenting.title
            , Url.Builder.string "timestamp" songLikingOrCommenting.timestamp
            , Url.Builder.string "user_identifier" userIdentifier
            ]
    in
    queryPairs
        |> Url.Builder.relative
            path
