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


type alias LatestFewSongs =
    List SongInfo


songinfo : Artist -> Title -> Time -> SongInfo
songinfo someArtist someTitle someTime =
    { artist = someArtist
    , time = someTime
    , title = someTitle
    }


latestFewSongsInit : LatestFewSongs
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


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { latestFew : LatestFewSongs
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model latestFewSongsInit [], Cmd.none )



-- UPDATE


type Msg
    = Add
    | Refresh
    | Send


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { latestFew, messages } =
    case msg of
        Add ->
            ( Model latestFew messages, Cmd.none )

        Refresh ->
            ( Model latestFew messages, Cmd.none )

        Send ->
            ( Model latestFew messages, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- VIEW


amazonConstant : String
amazonConstant =
    "http://www.amazon.com/s/ref=nb_sb_noss?tag=wtmdradio-20&url=search-alias%3Ddigital-music&field-keywords="


songPlayed : Model -> SongInfo -> Html Msg
songPlayed model song =
    div
        []
        [ p
            []
            [ button
                [ type_ "button" ]
                []
            , text song.time
            , a
                [ target "_blank"
                , href (amazonConstant ++ song.title ++ "+" ++ song.artist)
                ]
                []
            ]
        , p
            []
            [ text song.title ]
        , p
            []
            [ text song.artist ]
        ]


songsPlayed : Model -> List (Html Msg)
songsPlayed model =
    List.map (songPlayed model) model.latestFew


view : Model -> Html Msg
view model =
    section
        []
        [ div
            [ id "songs-remembered"
            , class "songs-played-or-remembered"
            ]
            (songsPlayed model)
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
