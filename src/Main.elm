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


module Main exposing (main)

-- import Html exposing (Html, button, div, text)
-- import Html.Attributes exposing (class, id)
-- import Html.Events exposing (..)
-- import List exposing (..)
-- import Maybe exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type Shape
    = Expanded
    | Shrunk


type alias Artist =
    String


type alias Time =
    String


type alias TimeStamp =
    String


type alias Title =
    String


type alias SongInfo =
    { artist : Artist
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongsList =
    List SongInfo


type alias Model =
    { shape : Shape
    , latestFew : SongsList
    , remembered : SongsList
    , messages : List String
    }


songinfo : Artist -> Title -> Time -> TimeStamp -> SongInfo
songinfo someArtist someTitle someTime someTimeStamp =
    { artist = someArtist
    , time = someTime
    , timeStamp = someTimeStamp
    , title = someTitle
    }


songsLatestFewInit : SongsList
songsLatestFewInit =
    [ songinfo "U2"
        "Bullet The Blue Sky"
        "5:53 PM"
        "2017 08 07 17 53"
    , songinfo "LP"
        "No Witness"
        "5:49 PM"
        "2017 08 07 17 49"
    , songinfo "Cage The Elephant"
        "Whole Wide World"
        "5:46 PM"
        "2017 08 07 17 46"
    , songinfo "Robert Randolph and the Fami"
        "Deliver Me"
        "5:41 PM"
        "2017 08 07 17 41"
    , songinfo "Outer Spaces"
        "Words"
        "5:31 PM"
        "2017 08 07 17 31"
    ]


songsRememberedInit : SongsList
songsRememberedInit =
    [ songinfo "The Rosebuds"
        "In My Teeth"
        "4:54 PM"
        "2017 08 07 16 54"
    , songinfo "T. Rex"
        "King Of The Rumbling Spires"
        "4:59 PM"
        "2017 08 07 16 59"
    , songinfo "Tedeschi Trucks Band"
        "I Pity The Fool - Live"
        "5:07 PM"
        "2017 08 07 17 07"
    , songinfo "Bobby \"Blue\" Bland"
        "I Pity The Fool"
        "5:14 PM"
        "2017 08 07 17 14"
    , songinfo "Eddy Clearwater"
        "Find You A Job"
        "5:19 PM"
        "2017 08 07 17 19"
    ]


init : ( Model, Cmd Msg )
init =
    --    ( Model Shrunk songsLatestFewInit songsRememberedInit []
    ( Model Expanded songsLatestFewInit songsRememberedInit []
    , Cmd.none
    )



-- UPDATE


type Msg
    = Add
    | Refresh
    | Send


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { shape, latestFew, remembered, messages } =
    case msg of
        Add ->
            ( Model shape latestFew remembered messages
            , Cmd.none
            )

        Refresh ->
            ( Model shape latestFew remembered messages
            , Cmd.none
            )

        Send ->
            ( Model shape latestFew remembered messages
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- VIEW


type SongGroup
    = Played
    | Remembered


amazonConstant : String
amazonConstant =
    "http://www.amazon.com/s/ref=nb_sb_noss?tag=wtmdradio-20&url=search-alias%3Ddigital-music&field-keywords="


buttonSong : SongGroup -> Int -> Html Msg
buttonSong songGroup index =
    let
        groupString : String
        groupString =
            case songGroup of
                Played ->
                    "Add"

                Remembered ->
                    "Drop"
    in
    button
        [ id ("button" ++ groupString ++ toString index)
        , type_ "button"
        ]
        []


styleCalc : SongGroup -> Int -> Int -> List (Attribute msg)
styleCalc songGroup lengthSongGroup index =
    let
        -- Golden ratio:
        -- https://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344
        goldenRatio : Float
        goldenRatio =
            0.6180339887498949

        base : Float
        base =
            16.0

        reversed : Int
        reversed =
            lengthSongGroup - index - 1

        sizeFactor : Float
        sizeFactor =
            case songGroup of
                Played ->
                    goldenRatio ^ toFloat index

                Remembered ->
                    goldenRatio ^ toFloat reversed

        fontSizeValue : String
        fontSizeValue =
            toString (sizeFactor * base) ++ "px"

        fontSizeStyling : List ( String, String )
        fontSizeStyling =
            [ ( "font-size", fontSizeValue ) ]

        saturation : Float
        saturation =
            sizeFactor * 0.5

        backgroundColorValue : String
        backgroundColorValue =
            "hsl(0,"
                ++ toString (saturation * 100.0)
                ++ "%,50%"

        backgroundColorStyling : List ( String, String )
        backgroundColorStyling =
            case songGroup of
                Played ->
                    []

                Remembered ->
                    [ ( "background-color", backgroundColorValue ) ]
    in
    [ style (backgroundColorStyling ++ fontSizeStyling) ]


songView : Model -> SongGroup -> Int -> SongInfo -> Html Msg
songView model songGroup index song =
    let
        buySong : List (Attribute msg)
        buySong =
            [ target "_blank"
            , href (amazonConstant ++ song.title ++ "+" ++ song.artist)
            ]

        lengthRemembered : Int
        lengthRemembered =
            List.length model.remembered

        groupAttributes : List (Attribute msg)
        groupAttributes =
            case model.shape of
                Expanded ->
                    []

                Shrunk ->
                    styleCalc songGroup lengthRemembered index
    in
    div
        groupAttributes
        [ p []
            [ buttonSong songGroup index
            , span []
                [ text song.time ]
            , a
                buySong
                []
            ]
        , p []
            [ text song.title ]
        , p []
            [ text song.artist ]
        ]


songsOfGroup : Model -> SongGroup -> List (Html Msg)
songsOfGroup model songGroup =
    let
        songs : List SongInfo
        songs =
            case songGroup of
                Played ->
                    model.latestFew

                Remembered ->
                    model.remembered
    in
    List.indexedMap (songView model songGroup) songs


buttonGroup : List (Html Msg)
buttonGroup =
    [ p []
        [ button
            [ type_ "button" ]
            []
        ]
    ]


songGroupToString : SongGroup -> String
songGroupToString songGroup =
    case songGroup of
        Played ->
            "played"

        Remembered ->
            "remembered"


divAttributes : SongGroup -> List (Attribute msg)
divAttributes songGroup =
    [ id ("songs-" ++ songGroupToString songGroup)
    , class "songs-group"
    ]


view : Model -> Html Msg
view model =
    main_ []
        [ section
            (divAttributes Remembered)
            (buttonGroup ++ songsOfGroup model Remembered)
        , hr [] []
        , section
            (divAttributes Played)
            (buttonGroup ++ songsOfGroup model Played)
        ]
