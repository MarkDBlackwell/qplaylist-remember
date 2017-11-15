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


module UpdateHelper
    exposing
        ( actionLikeOrComment2String
        , elmCycleDefault
        , likeOrCommentRequestUriText
        , relative
        , stateVector
        )

import ElmCycle
    exposing
        ( ElmCycle
        )
import ModelType
    exposing
        ( Model
        , Optional
            ( Closed
            , Open
            )
        )
import SongInitialize
    exposing
        ( likedOrCommentedInit
        , songLikingOrCommentingInit
        )
import SongType
    exposing
        ( SongRecentMaybe
        , SongRemembered
        , SongRememberedMaybe
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        )
import UpdateRequestType
    exposing
        ( ActionLikeOrComment
            ( Comment
            , Like
            )
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


elmCycleDefault : Model -> ElmCycle
elmCycleDefault model =
    ( model
    , focusInputPossibly model
    )


likeOrCommentRequestUriText : SongRememberedMaybe -> UserIdentifier -> LikeOrCommentText -> UriText
likeOrCommentRequestUriText songLikingOrCommentingMaybe userIdentifier likeOrCommentText =
    let
        artistTimeTitle : UriText
        artistTimeTitle =
            String.concat
                [ songLikingOrCommenting.time
                , " "
                , songLikingOrCommenting.artist
                , ": "
                , songLikingOrCommenting.title
                ]

        artistTimeTitlePrepended : UriText
        artistTimeTitlePrepended =
            String.cons
                'a'
                artistTimeTitle

        basename : UriText
        basename =
            "append.json"

        songLikingOrCommenting : SongRemembered
        songLikingOrCommenting =
            Maybe.withDefault songLikingOrCommentingInit songLikingOrCommentingMaybe
    in
    relative
        [ basename ]
        [ ( "user_identifier", userIdentifier )
        , ( "timestamp", songLikingOrCommenting.timestamp )
        , ( "song", artistTimeTitlePrepended )
        , ( "comment", likeOrCommentText )
        ]


relative : UrlBeforeQueryList -> QueryPairs -> UriText
relative urlBeforeQueryList queryPairs =
    --See:
    --https://github.com/elm-lang/http/issues/10
    --https://github.com/elm-lang/url
    --https://github.com/evancz/elm-http
    --http://package.elm-lang.org/packages/elm-lang/http/latest
    --TODO: When elm-lang/url is updated to contain 'relative',
    --consider replacing this code:
    let
        query : UriText
        query =
            let
                joinAndEscape : QueryPair -> UriText
                joinAndEscape ( name, value ) =
                    let
                        escapeAmpersands : UriText -> UriText
                        escapeAmpersands string =
                            String.split "&" string
                                |> String.join "%26"

                        escapeEqualsSigns : UriText -> UriText
                        escapeEqualsSigns string =
                            String.split "=" string
                                |> String.join "%3D"

                        escapeHashes : UriText -> UriText
                        escapeHashes string =
                            String.split "#" string
                                |> String.join "%23"
                    in
                    String.concat
                        [ name
                        , "="

                        --See:
                        --http://package.elm-lang.org/packages/elm-lang/http/latest/Http
                        --TODO: Possibly, use Http.encodeUri instead:
                        , value
                            |> escapeAmpersands
                            |> escapeEqualsSigns
                            |> escapeHashes
                        ]
            in
            if List.isEmpty queryPairs then
                ""
            else
                List.map joinAndEscape queryPairs
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
            maybeMapWithDefault Closed (\_ -> Open) model.songCommentingMaybe
    in
    ( model.awaitingServerResponse
    , commentOptional
    )
