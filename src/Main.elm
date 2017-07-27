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

import Html exposing (..)
import Html.Attributes exposing (..)
import Maybe exposing (..)


type alias Artist =
    String


type alias Time =
    String


type alias Title =
    String


type alias SongInfo =
    { artist : Artist
    , title : Title
    , time : Time
    }


type alias LatestFewSongs =
    List SongInfo


songinfo : Artist -> Title -> Time -> SongInfo
songinfo a b c =
    { artist = a
    , title = b
    , time = c
    }


latestFewSongsInit : LatestFewSongs
latestFewSongsInit =
    [ songinfo "LP"
        "No Witness"
        "5:53 PM"
    , songinfo "Outer Spaces"
        "Words"
        "5:49 PM"
    , songinfo "Cage The Elephant"
        "Whole Wide World"
        "5:46 PM"
    , songinfo "Robert Randolph and the Fami"
        "Deliver Me"
        "5:41 PM"
    , songinfo "U2"
        "Bullet The Blue Sky"
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
            ( Model latestFewSongsInit [], Cmd.none )

        Refresh ->
            ( Model latestFewSongsInit [], Cmd.none )

        Send ->
            ( Model latestFewSongsInit [], Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- VIEW


amazonConstant : String
amazonConstant =
    "http://www.amazon.com/s/ref=nb_sb_noss?tag=wtmdradio-20&url=search-alias%3Ddigital-music&field-keywords="


someArtist : Artist
someArtist =
    case List.head latestFewSongsInit of
        Nothing ->
            ""

        Just head ->
            head.artist


someTime : Time
someTime =
    case List.head latestFewSongsInit of
        Nothing ->
            ""

        Just head ->
            head.time


someTitle : Title
someTitle =
    case List.head latestFewSongsInit of
        Nothing ->
            ""

        Just head ->
            head.title


songPlayed : Model -> Html Msg
songPlayed model =
    div
        []
        [ p
            []
            [ button
                [ type_ "button" ]
                []
            , text someTime
            , a
                [ target "_blank"
                , href (amazonConstant ++ someTitle ++ "+" ++ someArtist)
                ]
                []
            ]
        , p
            []
            [ text someTitle ]
        , p
            []
            [ text someArtist ]
        ]


songsPlayed : Model -> Html Msg
songsPlayed model =
    songPlayed model


view : Model -> Html Msg
view model =
    section
        []
        [ div
            [ id "songs-played"
            , class "songs-played-or-remembered"
            ]
            [ button
                [ id "refresh"
                , type_ "button"
                ]
                []
            , songsPlayed model
            ]
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
