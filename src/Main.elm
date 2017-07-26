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


module Main exposing (..)

import Html exposing (..)
-- import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Artist =
    String


type alias Title =
    String


type alias Time =
    String


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
    { input : String
    , messages : List String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "" [], Cmd.none )



-- UPDATE


type Msg
    = Input String
    | Send
    | NewMessage String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { input, messages } =
    case msg of
        Input newInput ->
            ( Model newInput messages, Cmd.none )

        Send ->
            ( Model "" [], Cmd.none )

        NewMessage str ->
            ( Model input (str :: messages), Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
      []



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ onInput Input ] []
        , button [ onClick Send ] [ text "Send" ]
        , div [] (List.map viewMessage (List.reverse model.messages))
        ]


viewMessage : String -> Html msg
viewMessage msg =
    div [] [ text msg ]
