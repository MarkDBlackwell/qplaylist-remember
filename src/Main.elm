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


type alias Artist =
    String


type alias Time =
    String


type alias Title =
    String


type alias SongInfo =
    { artist : Artist
    , time : Time
    , title : Title
    }


type alias SongsList =
    List SongInfo


type alias Model =
    { remembered : SongsList
    , latestFew : SongsList
    , messages : List String
    }


songinfo : Artist -> Title -> Time -> SongInfo
songinfo someArtist someTitle someTime =
    { artist = someArtist
    , time = someTime
    , title = someTitle
    }


songsLatestFewInit : SongsList
songsLatestFewInit =
    [ songinfo "U2"
        "Bullet The Blue Sky"
        "5:53 PM"
    , songinfo "LP"
        "No Witness"
        "5:49 PM"
    , songinfo "Cage The Elephant"
        "Whole Wide World"
        "5:46 PM"
    , songinfo "Robert Randolph and the Fami"
        "Deliver Me"
        "5:41 PM"
    , songinfo "Outer Spaces"
        "Words"
        "5:31 PM"
    ]


songsRememberedInit : SongsList
songsRememberedInit =
    [ songinfo "The Rosebuds"
        "In My Teeth"
        "4:54 PM"
    , songinfo "T. Rex"
        "King Of The Rumbling Spires"
        "4:59 PM"
    , songinfo "Tedeschi Trucks Band"
        "I Pity The Fool - Live"
        "5:07 PM"
    , songinfo "Bobby \"Blue\" Bland"
        "I Pity The Fool"
        "5:14 PM"
    , songinfo "Eddy Clearwater"
        "Find You A Job"
        "5:19 PM"
    ]


init : ( Model, Cmd Msg )
init =
    ( Model songsRememberedInit songsLatestFewInit [], Cmd.none )



-- UPDATE


type Msg
    = Add
    | Refresh
    | Send


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { remembered, latestFew, messages } =
    case msg of
        Add ->
            ( Model remembered latestFew messages, Cmd.none )

        Refresh ->
            ( Model remembered latestFew messages, Cmd.none )

        Send ->
            ( Model remembered latestFew messages, Cmd.none )



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


buttonMy : SongGroup -> Int -> Html Msg
buttonMy songGroup index =
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


styleCalc : SongGroup -> Float -> List (Attribute msg)
styleCalc songGroup sizeFactor =
    let
        base : Float
        base =
            10.0

        size : String
        size =
            toString (sizeFactor * base) ++ "px"
    in
    case songGroup of
        Played ->
            []

        Remembered ->
            [ style
                [ ( "font-size", size )
                ]
            ]



-- Golden ratio:
-- https://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344


goldenRatio : Float
goldenRatio =
    0.6180339887498949


songView : Model -> SongGroup -> Int -> SongInfo -> Html Msg
songView model songGroup index song =
    let
        buySong : List (Attribute msg)
        buySong =
            [ target "_blank"
            , href (amazonConstant ++ song.title ++ "+" ++ song.artist)
            ]

        visualEqualityFactor : Float
        visualEqualityFactor =
            0.97

        length : Int
        length =
            List.length model.remembered

        reversed : Int
        reversed =
            length - index - 1

        factor : Float
        factor =
            (goldenRatio ^ toFloat (reversed - 1)) * visualEqualityFactor
    in
    div
        (styleCalc songGroup factor)
        [ p
            []
            [ buttonMy songGroup index
            , text song.time
            , a
                buySong
                []
            ]
        , p
            []
            [ text song.title ]
        , p
            []
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
    , class "songs-played-or-remembered"
    ]


view : Model -> Html Msg
view model =
    main_
        []
        [ section
            (divAttributes Remembered)
            (songsOfGroup model Remembered)
        , hr [] []
        , section
            (divAttributes Played)
            ([ button
                [ id "refresh"
                , type_ "button"
                ]
                []
             ]
                ++ songsOfGroup model Played
            )
        ]
