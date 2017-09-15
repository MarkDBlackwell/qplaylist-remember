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
            case group of
                Played ->
                    []

                Remembered ->
                    [ ( "background-color", backgroundColorValue ) ]

        backgroundColorValue : String
        backgroundColorValue =
            "hsl(0,"
                ++ toString (saturation * 100.0)
                ++ "%,50%"

        base : Float
        base =
            16.0

        fontSizeStyling : List ( String, String )
        fontSizeStyling =
            [ ( "font-size", fontSizeValue ) ]

        fontSizeValue : String
        fontSizeValue =
            toString (sizeFactor * base)
                ++ "px"

        songLatestFewOrRememberedIndexReversed : SongLatestFewOrRememberedIndex
        songLatestFewOrRememberedIndexReversed =
            songGroupLength - songLatestFewOrRememberedIndex - 1

        saturation : Float
        saturation =
            sizeFactor * 0.5

        sizeFactor : Float
        sizeFactor =
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
