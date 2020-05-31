{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module UpdateHelper exposing
    ( actionLikeOrComment2String
    , elmCycleDefault
    , likeOrCommentRequestUriText
    , relative
    , stateVector
    )

import ElmCycle
import ModelType
    exposing
        ( Model
        , Optional(..)
        )
import SongInitialize
import SongType
    exposing
        ( SongRecentMaybe
        , SongRemembered
        , SongRememberedMaybe
        )
import UpdateFocus
import UpdateRequestType
    exposing
        ( ActionLikeOrComment(..)
        , AwaitingServerResponse
        , LikeOrCommentText
        , QueryPair
        , QueryPairs
        , UriText
        , UrlBeforeQueryList
        )
import UserIdentifierType
    exposing
        ( UserIdentifier
        )
import Utilities
    exposing
        ( maybeMapWithDefault
        )



-- UPDATE


actionLikeOrComment2String : ActionLikeOrComment -> String
actionLikeOrComment2String actionLikeOrComment =
    case actionLikeOrComment of
        Comment ->
            "Comment"

        Like ->
            "Like"


elmCycleDefault : Model -> ElmCycle.ElmCycle
elmCycleDefault model =
    ( model
    , UpdateFocus.cmdFocusInputPossibly model
    )


likeOrCommentRequestUriText : UserIdentifier -> SongRememberedMaybe -> UriText -> LikeOrCommentText -> UriText
likeOrCommentRequestUriText userIdentifier songLikingOrCommentingMaybe commentCategory likeOrCommentText =
    let
        basename : UriText
        basename =
            "append.json"

        songCategory : UriText
        songCategory =
            "s"

        songLikingOrCommenting : SongRemembered
        songLikingOrCommenting =
            songLikingOrCommentingMaybe
                |> Maybe.withDefault SongInitialize.songLikingOrCommentingInit
    in
    relative
        [ basename ]
        [ ( "comment", likeOrCommentText )
        , ( "comment_category", commentCategory )
        , ( "song_artist", songLikingOrCommenting.artist )
        , ( "song_category", songCategory )
        , ( "song_time", songLikingOrCommenting.time )
        , ( "song_title", songLikingOrCommenting.title )
        , ( "timestamp", songLikingOrCommenting.timestamp )
        , ( "user_identifier", userIdentifier )
        ]


relative : UrlBeforeQueryList -> QueryPairs -> UriText
relative urlBeforeQueryList queryPairs =
    --See:
    --  http://package.elm-lang.org/packages/elm/http/2.0.0/
    --TODO: When elm-lang/url is updated to contain 'relative',
    --    consider replacing this code:
    let
        query : UriText
        query =
            let
                joinAndEscape : QueryPair -> UriText
                joinAndEscape ( name, value ) =
                    let
                        escapeAmpersands : UriText -> UriText
                        escapeAmpersands string =
                            string
                                |> String.split "&"
                                |> String.join "%26"

                        escapeEqualsSigns : UriText -> UriText
                        escapeEqualsSigns string =
                            string
                                |> String.split "="
                                |> String.join "%3D"

                        escapeHashes : UriText -> UriText
                        escapeHashes string =
                            string
                                |> String.split "#"
                                |> String.join "%23"
                    in
                    [ name
                    , "="

                    --See:
                    --  http://package.elm-lang.org/packages/elm/http/2.0.0/
                    --  http://github.com/elm/http/issues/10#issuecomment-436681699
                    --  http://package.elm-lang.org/packages/elm/url/1.0.0/
                    --TODO: Possibly, use Http.encodeUri instead:
                    --    or maybe encodeUri has been superseded.
                    , value
                        |> escapeAmpersands
                        |> escapeEqualsSigns
                        |> escapeHashes
                    ]
                        |> String.concat
            in
            if List.isEmpty queryPairs then
                ""

            else
                queryPairs
                    |> List.map joinAndEscape
                    |> String.join "&"
                    |> String.cons '?'

        urlBeforeQuery : UriText
        urlBeforeQuery =
            urlBeforeQueryList
                |> String.join "/"
    in
    String.concat
        [ urlBeforeQuery
        , query
        ]


stateVector : Model -> ( AwaitingServerResponse, Optional )
stateVector model =
    let
        commentOptional : Optional
        commentOptional =
            model.songCommentingMaybe
                |> maybeMapWithDefault Closed (\_ -> Open)
    in
    ( model.awaitingServerResponse
    , commentOptional
    )
