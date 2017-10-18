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


module ViewBuySong
    exposing
        ( buySongAnchor
        )

import ElmCycle
    exposing
        ( Msg
            ( None
            )
        )
import Html
    exposing
        ( Html
        , a
        )
import Html.Attributes
    exposing
        ( href
        , target
        , title
        )
import Html.Events
    exposing
        ( onClick
        )
import SongType
    exposing
        ( SongRemembered
        )
import UpdateHelper
    exposing
        ( relative
        )
import UpdateRequestType
    exposing
        ( QueryPairs
        , UriText
        , UrlBeforeQueryList
        )
import Utilities
    exposing
        ( innerHtmlEmpty
        )
import ViewType
    exposing
        ( HoverText
        )


-- VIEW


buySongAnchor : SongRemembered -> Html Msg
buySongAnchor song =
    let
        hoverText : HoverText
        hoverText =
            "See this song on Amazon (in new tab)"

        uriText : UriText
        uriText =
            let
                urlBeforeQueryList : UrlBeforeQueryList
                urlBeforeQueryList =
                    [ "http://www.amazon.com/s/ref=nb_sb_noss" ]

                queryPairs : QueryPairs
                queryPairs =
                    let
                        fieldKeywords : UriText
                        fieldKeywords =
                            [ song.title
                            , song.artist
                            ]
                                |> String.join "+"
                    in
                    [ ( "tag", "wtmdradio-20" )
                    , ( "url", "search-alias=digital-music" )
                    , ( "field-keywords"
                      , fieldKeywords
                      )
                    ]
            in
            relative urlBeforeQueryList queryPairs
    in
    a
        [ href uriText
        , onClick None
        , target "_blank"
        , title hoverText
        ]
        innerHtmlEmpty
