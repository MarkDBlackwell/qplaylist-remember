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

-- import Html exposing (Html, button, div, input, text)
-- import Html.Attributes exposing (class, id)

import Html exposing (..)
import Html.Attributes exposing (..)


-- import Html.Events exposing (..)


type alias Artist =
    String


type alias Title =
    String


type alias Time =
    String


type alias SongInfo =
    { artist : Artist
    , title : Title
    , time : Time
    }


type alias LatestFiveInit =
    List SongInfo


songinfo : Artist -> Title -> Time -> SongInfo
songinfo a b c =
    { artist = a
    , title = b
    , time = c
    }


latestFiveInit : LatestFiveInit
latestFiveInit =
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
    { latestFive : LatestFiveInit
    , input : String
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model latestFiveInit "" [], Cmd.none )



-- UPDATE


type Msg
    = Send


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { latestFive, input, messages } =
    case msg of
        Send ->
            ( Model latestFiveInit "" [], Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- VIEW


amazonSteady : String
amazonSteady =
    "http://www.amazon.com/s/ref=nb_sb_noss?tag=wtmdradio-20&url=search-alias%3Ddigital-music&field-keywords="


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
            , div
                []
                [ p
                    []
                    [ button
                        [ type_ "button" ]
                        []
                    , text "Time t"
                    , a
                        [ target "_blank"
                        , href (amazonSteady ++ "Some title t+Some artist t")
                        ]
                        []
                    ]
                , p
                    []
                    [ text "Some title t" ]
                , p
                    []
                    [ text "Some artist t" ]
                ]
            ]
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]



{-
         type alias SongInfo =
             ( Artist, Title, Time )


         latestFive : List SongInfo
         latestFive =
             [ ( "LP"
               , "No Witness"
               , "5:53 PM"
               )
             , ( "Outer Spaces"
               , "Words"
               , "5:49 PM"
               )
             , ( "Cage The Elephant"
               , "Whole Wide World"
               , "5:46 PM"
               )
             , ( "Robert Randolph and the Fami"
               , "Deliver Me"
               , "5:41 PM"
               )
             , ( "U2"
               , "Bullet The Blue Sky"
               , "5:31 PM"
               )
             ]


      songinfoTwo : a -> b -> c -> { artist : a, time : c, title : b }
      songinfoTwo a b c =
          { artist = a
          , title = b
          , time = c
          }


   latestFive : List SongInfo
   latestFive =
       [ { artist = "LP"
         , title = "No Witness"
         , time = "5:53 PM"
         }
       , { artist = "Outer Spaces"
         , title = "Words"
         , time = "5:49 PM"
         }
       , { artist = "Cage The Elephant"
         , title = "Whole Wide World"
         , time = "5:46 PM"
         }
       , { artist = "Robert Randolph and the Fami"
         , title = "Deliver Me"
         , time = "5:41 PM"
         }
       , { artist = "U2"
         , title = "Bullet The Blue Sky"
         , time = "5:31 PM"
         }
       ]


-}
