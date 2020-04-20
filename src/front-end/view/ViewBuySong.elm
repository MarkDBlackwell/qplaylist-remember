{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ViewBuySong exposing (buySongAnchor)

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
