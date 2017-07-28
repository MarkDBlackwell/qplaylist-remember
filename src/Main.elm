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


latestFewSongsInit : SongsList
latestFewSongsInit =
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


rememberedSongsInit : SongsList
rememberedSongsInit =
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
    ( Model rememberedSongsInit latestFewSongsInit [], Cmd.none )



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


amazonConstant : String
amazonConstant =
    "http://www.amazon.com/s/ref=nb_sb_noss?tag=wtmdradio-20&url=search-alias%3Ddigital-music&field-keywords="


buttonMy : String -> Int -> Html Msg
buttonMy group index =
    button
        [ id ("button" ++ group ++ toString index)
        , type_ "button"
        ]
        []


styleCalc : String -> Float -> Float -> List (Attribute msg)
styleCalc group sizeFactor base =
    let
        fontSize =
            toString (sizeFactor * base) ++ "px"
    in
    if "Drop" /= group then
        []
    else
        [ style
            [ ( "font-size", fontSize )
            ]
        ]


songPlayedOrRemembered : Model -> String -> Int -> SongInfo -> Html Msg
songPlayedOrRemembered model group index song =
    let
        length =
            List.length model.remembered

        reversed =
            length - 1 - index

        factor =
            1.0 - (0.25 * toFloat reversed)
    in
    div
        []
        [ p
            (styleCalc group factor 15.0)
            [ buttonMy group index
            , text song.time
            , a
                [ target "_blank"
                , href (amazonConstant ++ song.title ++ "+" ++ song.artist)
                ]
                []
            ]
        , p
            (styleCalc group factor 12.0)
            [ text song.title ]
        , p
            (styleCalc group factor 12.0)
            [ text song.artist ]
        ]


songPlayed : Model -> Int -> SongInfo -> Html Msg
songPlayed model index song =
    songPlayedOrRemembered model "Add" index song


songRemembered : Model -> Int -> SongInfo -> Html Msg
songRemembered model index song =
    songPlayedOrRemembered model "Drop" index song


songsPlayed : Model -> List (Html Msg)
songsPlayed model =
    List.indexedMap (songPlayed model) model.latestFew


songsRemembered : Model -> List (Html Msg)
songsRemembered model =
    List.indexedMap (songRemembered model) model.remembered


view : Model -> Html Msg
view model =
    section
        []
        [ div
            [ id "songs-remembered"
            , class "songs-played-or-remembered"
            ]
            (songsRemembered model)
        , hr [] []
        , div
            [ id "songs-played"
            , class "songs-played-or-remembered"
            ]
            ([ button
                [ id "refresh"
                , type_ "button"
                ]
                []
             ]
                ++ songsPlayed model
            )
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
