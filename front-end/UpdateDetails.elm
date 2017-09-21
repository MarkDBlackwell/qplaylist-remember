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


module UpdateDetails
    exposing
        ( focusInputPossibly
        , likeOrCommentRequestUriText
        , relative
        )

import Message
    exposing
        ( Msg
        )
import Model
    exposing
        ( LikeOrCommentText
        , Model
        )
import ModelUpdate
    exposing
        ( QueryBeforeList
        , QueryPair
        , QueryPairs
        , UriText
        )
import Song
    exposing
        ( SongLikingOrCommenting
        )
import UpdateUtilities
    exposing
        ( focusSet
        )
import UserIdentifier
    exposing
        ( UserIdentifier
        )


-- UPDATE


focusInputPossibly : Model -> Cmd Msg
focusInputPossibly model =
    case model.songCommenting of
        Nothing ->
            Cmd.none

        _ ->
            focusSet "input"


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
