{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module BuySongView exposing (buySongAnchor)

import ElmCycle
    exposing
        ( Msg(..)
        )
import Html
import Html.Attributes
import Html.Events
import RequestUpdateType
    exposing
        ( QueryPairs
        , UrlBeforeQueryList
        , UrlText
        )
import SongType
    exposing
        ( SongRemembered
        )
import Url.Builder
import Utilities
    exposing
        ( innerHtmlEmpty
        )
import ViewType
    exposing
        ( HoverText
        )



-- VIEW


buySongAnchor : SongRemembered -> Html.Html ElmCycle.Msg
buySongAnchor song =
    let
        hoverText : HoverText
        hoverText =
            "See this song on Amazon (in new tab)"

        urlText : UrlText
        urlText =
            let
                origin : String
                origin =
                    "www.amazon.com"
                        |> String.append "http://"

                path : List String
                path =
                    [ "s"
                    , "ref=nb_sb_noss"
                    ]

                queryPairs : QueryPairs
                queryPairs =
                    let
                        fieldKeywords : UrlText
                        fieldKeywords =
                            [ song.title
                            , song.artist
                            ]
                                |> String.join "+"
                    in
                    [ Url.Builder.string "tag" "wtmdradio-20"
                    , Url.Builder.string "url" "search-alias=digital-music"
                    , Url.Builder.string "field-keywords" fieldKeywords
                    ]
            in
            --"http://www.amazon.com/s/ref=nb_sb_noss"
            queryPairs
                |> Url.Builder.crossOrigin
                    origin
                    path
    in
    Html.a
        [ Html.Attributes.href urlText
        , Html.Events.onClick MsgNone
        , Html.Attributes.target "_blank"
        , Html.Attributes.title hoverText
        ]
        innerHtmlEmpty
