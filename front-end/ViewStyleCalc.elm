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


module ViewStyleCalc exposing (styleCalc)

import Html exposing (Attribute)
import Html.Attributes exposing (style)
import ModelDetailsView
    exposing
        ( SongGroup
            ( Played
            , Remembered
            )
        , SongGroupLength
        , SongLatestFewOrRememberedIndex
        )
import ViewUtilities exposing (goldenRatio)


-- VIEW


styleCalc : SongGroup -> SongGroupLength -> SongLatestFewOrRememberedIndex -> Attribute msg
styleCalc group songGroupLength songLatestFewOrRememberedIndex =
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
                    "hsl(0,"
                        ++ toString (saturation * 100.0)
                        ++ "%,50%"
            in
            case group of
                Played ->
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
                    toString (scaleFactor * base)
                        ++ "rem"
            in
            [ ( "font-size", fontSizeValue ) ]

        scaleFactor : Float
        scaleFactor =
            let
                songLatestFewOrRememberedIndexReversed : SongLatestFewOrRememberedIndex
                songLatestFewOrRememberedIndexReversed =
                    songGroupLength - songLatestFewOrRememberedIndex - 1
            in
            case group of
                Played ->
                    goldenRatio ^ toFloat songLatestFewOrRememberedIndex

                Remembered ->
                    goldenRatio ^ toFloat songLatestFewOrRememberedIndexReversed
    in
    style
        (backgroundColorStyling
            ++ fontSizeStyling
        )
