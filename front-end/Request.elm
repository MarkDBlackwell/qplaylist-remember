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


module Request
    exposing
        ( AwaitingServerResponse
        , HttpRequestOrResponseText
        , HttpRequestText
        , HttpResponseText
        , QueryBeforeList
        , QueryPairs
        , RequestOrResponseLabelText
        , UriText
        , likeOrCommentRequestUriText
        , relative
        )

import Song
    exposing
        ( SongLikingOrCommenting
        )
import UserIdentifier
    exposing
        ( UserIdentifier
        )


-- MODEL


type alias AwaitingServerResponse =
    Bool


type alias RequestOrResponseLabelText =
    String



-- UPDATE


type alias HttpRequestText =
    String


type alias HttpRequestOrResponseText =
    String


type alias HttpResponseText =
    String


type alias LikeOrCommentText =
    String


type alias QueryBeforeList =
    --See:
    --https://github.com/elm-lang/url
    --https://tools.ietf.org/html/rfc3986
    --When joined, then comprises a URI's scheme, authority, and path:
    List UriText


type alias QueryPair =
    ( UriText, UriText )


type alias QueryPairs =
    List QueryPair


type alias UriText =
    String


likeOrCommentRequestUriText : SongLikingOrCommenting -> UserIdentifier -> LikeOrCommentText -> UriText
likeOrCommentRequestUriText songLikingOrCommenting userIdentifier likeOrCommentText =
    let
        artistTimeTitle : UriText
        artistTimeTitle =
            case songLikingOrCommenting of
                Nothing ->
                    ""

                Just songLikingOrCommenting ->
                    songLikingOrCommenting.time
                        ++ " "
                        ++ songLikingOrCommenting.artist
                        ++ ": "
                        ++ songLikingOrCommenting.title

        basename : UriText
        basename =
            "append.json"

        timestamp : UriText
        timestamp =
            case songLikingOrCommenting of
                Nothing ->
                    ""

                Just songLikingOrCommenting ->
                    songLikingOrCommenting.timestamp
    in
    relative
        [ basename ]
        [ ( "user_identifier", userIdentifier )
        , ( "timestamp", timestamp )
        , ( "song", artistTimeTitle )
        , ( "comment", likeOrCommentText )
        ]


relative : QueryBeforeList -> QueryPairs -> UriText
relative queryBeforeList queryPairs =
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
                queryPairJoin : QueryPair -> UriText
                queryPairJoin ( name, value ) =
                    let
                        escapeAll : UriText -> UriText
                        escapeAll string =
                            let
                                escapeAmpersands : UriText -> UriText
                                escapeAmpersands string =
                                    String.join
                                        "%26"
                                        (String.split "&" string)

                                escapeEqualsSigns : UriText -> UriText
                                escapeEqualsSigns string =
                                    String.join
                                        "%3D"
                                        (String.split "=" string)

                                escapeHashes : UriText -> UriText
                                escapeHashes string =
                                    String.join
                                        "%23"
                                        (String.split "#" string)
                            in
                            --See:
                            --http://package.elm-lang.org/packages/elm-lang/http/latest/Http
                            --TODO: Possibly, use Http.encodeUri instead:
                            escapeHashes (escapeEqualsSigns (escapeAmpersands string))
                    in
                    name ++ "=" ++ escapeAll value
            in
            String.join
                "&"
                (List.map queryPairJoin queryPairs)

        queryBefore : UriText
        queryBefore =
            String.join
                "/"
                queryBeforeList

        queryStarter : UriText
        queryStarter =
            if String.isEmpty query then
                ""
            else
                "?"
    in
    queryBefore
        ++ queryStarter
        ++ query
