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
        ( ActionName
            ( ActionDecoding
            , ActionRequest
            , ActionResponse
            )
        , AwaitingServerResponse
        , HttpRequestOrResponseTextMaybe
        , HttpRequestText
        , HttpResponseText
        , QueryBeforeList
        , QueryPairs
        , UriText
        , actionName2String
        , likeOrCommentRequestUriText
        , relative
        )

import Song
    exposing
        ( SongLikingOrCommenting
        , SongLikingOrCommentingMaybe
        , songLikingOrCommentingConstructor
        )
import UserIdentifier
    exposing
        ( UserIdentifier
        )


-- MODEL


type alias AwaitingServerResponse =
    Bool



-- UPDATE


type ActionName
    = ActionDecoding
    | ActionRequest
    | ActionResponse


type alias HttpRequestText =
    String


type alias HttpRequestOrResponseText =
    String


type alias HttpRequestOrResponseTextMaybe =
    Maybe HttpRequestOrResponseText


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


actionName2String : ActionName -> String
actionName2String actionName =
    case actionName of
        ActionDecoding ->
            "Decoding"

        ActionRequest ->
            "Request"

        ActionResponse ->
            "Response"


likeOrCommentRequestUriText : SongLikingOrCommentingMaybe -> UserIdentifier -> LikeOrCommentText -> UriText
likeOrCommentRequestUriText songLikingOrCommentingMaybe userIdentifier likeOrCommentText =
    let
        artistTimeTitle : UriText
        artistTimeTitle =
            String.concat
                [ song.time
                , " "
                , song.artist
                , ": "
                , song.title
                ]

        basename : UriText
        basename =
            "append.json"

        song : SongLikingOrCommenting
        song =
            Maybe.withDefault
                (songLikingOrCommentingConstructor "" "" "" "")
                songLikingOrCommentingMaybe
    in
    relative
        [ basename ]
        [ ( "user_identifier", userIdentifier )
        , ( "timestamp", song.timestamp )
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
                    String.concat
                        [ name
                        , "="
                        , escapeAll value
                        ]
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
