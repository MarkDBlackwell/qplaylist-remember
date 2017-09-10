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


module ViewUtilities
    exposing
        ( goldenRatio
        , htmlNodeNull
        , relative
        , showCommentButtons
        , songGroup2String
        )

import Html
    exposing
        ( Html
        , text
        )
import MessageDetails exposing (Msg)
import ModelDetailsUpdate
    exposing
        ( QueryBeforeList
        , QueryPair
        , QueryPairs
        , UriText
        )
import ModelDetailsView
    exposing
        ( SongGroup
            ( Played
            , Remembered
            )
        )


-- VIEW


goldenRatio : Float
goldenRatio =
    --See:
    --https://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344
    0.6180339887498949


htmlNodeNull : Html Msg
htmlNodeNull =
    text ""


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
        escapeAll : UriText -> UriText
        escapeAll string =
            --See:
            --http://package.elm-lang.org/packages/elm-lang/http/latest/Http
            --TODO: Possibly, use Http.encodeUri instead:
            escapeHashes (escapeEqualsSigns (escapeAmpersands string))

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

        query : UriText
        query =
            String.join
                "&"
                (List.map queryPairJoin queryPairs)

        queryBefore : UriText
        queryBefore =
            String.join
                "/"
                queryBeforeList

        queryPairJoin : QueryPair -> UriText
        queryPairJoin ( name, value ) =
            String.join
                "="
                [ name
                , escapeAll value
                ]
    in
    queryBefore ++ "?" ++ query


showCommentButtons : Bool
showCommentButtons =
    --TODO: Possibly move to model.
    True


songGroup2String : SongGroup -> String
songGroup2String group =
    case group of
        Played ->
            "played"

        Remembered ->
            "remembered"
