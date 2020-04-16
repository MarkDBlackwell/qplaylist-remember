{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ViewStyleCalc
    exposing
        ( styleCalc
        )

import Html
    exposing
        ( Attribute
        )
import Html.Attributes
    exposing
        ( style
        )
import SongType
    exposing
        ( SongGroup
            ( Recent
            , Remembered
            )
        , SongGroupLength
        , SongsRecentOrRememberedIndex
        )
import Utilities
    exposing
        ( goldenRatio
        )


-- VIEW


styleCalc : SongGroup -> SongGroupLength -> SongsRecentOrRememberedIndex -> Attribute msg
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
                            |> toString
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
                            |> toString
                        , "rem"
                        ]
            in
            [ ( "font-size", fontSizeValue ) ]

        scaleFactor : Float
        scaleFactor =
            let
                songsRecentOrRememberedIndexReversed : SongsRecentOrRememberedIndex
                songsRecentOrRememberedIndexReversed =
                    songGroupLength - songsRecentOrRememberedIndex - 1
            in
            case group of
                Recent ->
                    goldenRatio ^ toFloat songsRecentOrRememberedIndex

                Remembered ->
                    goldenRatio ^ toFloat songsRecentOrRememberedIndexReversed
    in
    style
        (List.concat
            [ backgroundColorStyling
            , fontSizeStyling
            ]
        )