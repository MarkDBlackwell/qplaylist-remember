{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ViewStyleCalc exposing (styleCalc)

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
        , pred
        )



-- VIEW


styleCalc : SongGroup -> SongGroupLength -> SongsRecentOrRememberedIndex -> List (Html.Attribute msg)
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
                    String.concat
                        [ "hsl(0,"
                        , saturation
                            * 100.0
                            |> String.fromFloat
                        , "%,50%"
                        ]
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
                    String.concat
                        [ scaleFactor
                            * base
                            |> String.fromFloat
                        , "rem"
                        ]
            in
            [ ( "font-size", fontSizeValue ) ]

        scaleFactor : Float
        scaleFactor =
            let
                songsRecentOrRememberedIndexReversed : SongsRecentOrRememberedIndex
                songsRecentOrRememberedIndexReversed =
                    songGroupLength
                        - songsRecentOrRememberedIndex
                        |> pred
            in
            case group of
                Recent ->
                    goldenRatio ^ toFloat songsRecentOrRememberedIndex

                Remembered ->
                    goldenRatio ^ toFloat songsRecentOrRememberedIndexReversed
    in
    List.concat
        [ backgroundColorStyling
        , fontSizeStyling
        ]
        |> List.map (\( property, value ) -> Html.Attributes.style property value)
