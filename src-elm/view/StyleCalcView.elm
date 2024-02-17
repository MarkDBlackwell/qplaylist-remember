{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module StyleCalcView exposing (styleCalc)

import ElmCycle
import Html
import Html.Attributes
import SongType
    exposing
        ( SongGroup(..)
        , SongGroupLength
        , SongsRecentOrRememberedIndex
        )
import Utilities
    exposing
        ( goldenRatio
        , succ
        )



-- VIEW


styleCalc : SongGroup -> SongGroupLength -> SongsRecentOrRememberedIndex -> List (Html.Attribute ElmCycle.Msg)
styleCalc group songGroupLength songsRecentOrRememberedIndex =
    let
        backgroundColorStyling : List ( String, String )
        backgroundColorStyling =
            let
                backgroundColorValue : String
                backgroundColorValue =
                    let
                        saturation : Float
                        saturation =
                            scaleFactor * 0.5
                    in
                    [ "hsl (0, "
                    , saturation
                        * 100.0
                        |> String.fromFloat
                    , "%, 50%"
                    ]
                        |> String.concat
                        |> String.replace " " ""
            in
            case group of
                Recent ->
                    []

                Remembered ->
                    [ ( "background-color", backgroundColorValue ) ]

        fontSizeStyling : List ( String, String )
        fontSizeStyling =
            let
                fontSizeValue : String
                fontSizeValue =
                    let
                        base : Float
                        base =
                            1.0666666666666667
                    in
                    [ scaleFactor
                        * base
                        |> String.fromFloat
                    , "rem"
                    ]
                        |> String.concat
            in
            [ ( "font-size", fontSizeValue ) ]

        scaleFactor : Float
        scaleFactor =
            let
                songsRecentOrRememberedIndexReversed : SongsRecentOrRememberedIndex
                songsRecentOrRememberedIndexReversed =
                    songsRecentOrRememberedIndex
                        |> succ
                        |> (-) songGroupLength
            in
            case group of
                Recent ->
                    songsRecentOrRememberedIndex
                        |> toFloat
                        |> (^) goldenRatio

                Remembered ->
                    songsRecentOrRememberedIndexReversed
                        |> toFloat
                        |> (^) goldenRatio
    in
    List.concat
        [ backgroundColorStyling
        , fontSizeStyling
        ]
        |> List.map
            (\( property, value ) -> Html.Attributes.style property value)
