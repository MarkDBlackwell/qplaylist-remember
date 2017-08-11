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

import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , em
        , hr
        , main_
        , p
        , section
        , span
        , text
        )
import Html.Attributes
    exposing
        ( class
        , href
        , id
        , style
        , target
        , type_
        )
import Html.Events exposing (onClick)


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


type alias TimeStamp =
    String


type alias Title =
    String


type alias Commented =
    Bool


type alias SongInfo =
    { artist : Artist
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    , commented : Commented
    }


type alias SongsList =
    List SongInfo


type alias Messages =
    List String


type alias Model =
    { shape : Shape
    , latestFew : SongsList
    , remembered : SongsList
    , messages : Messages
    }


type Shape
    = Expanded
    | Shrunk


songinfo : Artist -> Title -> Time -> TimeStamp -> Commented -> SongInfo
songinfo artist title time timeStamp commented =
    { artist = artist
    , title = title
    , time = time
    , timeStamp = timeStamp
    , commented = commented
    }


messagesInit : Messages
messagesInit =
    []


songsLatestFewInit : SongsList
songsLatestFewInit =
    [ songinfo "U2"
        "Bullet The Blue Sky"
        "5:53 PM"
        "2017 08 07 17 53"
        False
    , songinfo "LP"
        "No Witness"
        "5:49 PM"
        "2017 08 07 17 49"
        False
    , songinfo "Cage The Elephant"
        "Whole Wide World"
        "5:46 PM"
        "2017 08 07 17 46"
        False
    , songinfo "Robert Randolph and the Fami"
        "Deliver Me"
        "5:41 PM"
        "2017 08 07 17 41"
        False
    , songinfo "Outer Spaces"
        "Words"
        "5:31 PM"
        "2017 08 07 17 31"
        False
    ]


songsLatestFewInitEmpty : SongsList
songsLatestFewInitEmpty =
    []


songsRememberedInit : SongsList
songsRememberedInit =
    [ songinfo "The Rosebuds"
        "In My Teeth"
        "4:54 PM"
        "2017 08 07 16 54"
        True
    , songinfo "T. Rex"
        "King Of The Rumbling Spires"
        "4:59 PM"
        "2017 08 07 16 59"
        True
    , songinfo "Tedeschi Trucks Band"
        "I Pity The Fool - Live"
        "5:07 PM"
        "2017 08 07 17 07"
        True
    , songinfo "Bobby \"Blue\" Bland"
        "I Pity The Fool"
        "5:14 PM"
        "2017 08 07 17 14"
        False
    , songinfo "Eddy Clearwater"
        "Find You A Job"
        "5:19 PM"
        "2017 08 07 17 19"
        True
    ]


songsRememberedInitEmpty : SongsList
songsRememberedInitEmpty =
    []


init : ( Model, Cmd Msg )
init =
    ( Model Shrunk songsLatestFewInit songsRememberedInit messagesInit
    , Cmd.none
    )



-- UPDATE


type Msg
    = Morph
    | Refresh


update : Msg -> Model -> ( Model, Cmd Msg )
update msg { shape, latestFew, remembered, messages } =
    let
        shapeNew : Shape
        shapeNew =
            case shape of
                Expanded ->
                    Shrunk

                Shrunk ->
                    Expanded
    in
    case msg of
        Morph ->
            ( Model shapeNew latestFew remembered messages
            , Cmd.none
            )

        Refresh ->
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


styleCalc : SongGroup -> Int -> Int -> List (Attribute msg)
styleCalc group lengthSongGroup index =
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
            case group of
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
            case group of
                Played ->
                    []

                Remembered ->
                    [ ( "background-color", backgroundColorValue ) ]
    in
    [ style (backgroundColorStyling ++ fontSizeStyling) ]


groupAttributes : SongGroup -> List (Attribute msg)
groupAttributes group =
    let
        groupString : String
        groupString =
            case group of
                Played ->
                    "played"

                Remembered ->
                    "remembered"
    in
    [ id ("songs-" ++ groupString)
    , class "songs-group"
    ]


songsOfGroup : Model -> SongGroup -> List (Html Msg)
songsOfGroup model group =
    let
        songs : List SongInfo
        songs =
            case group of
                Played ->
                    model.latestFew

                Remembered ->
                    model.remembered
    in
    List.indexedMap (songView model group) songs


buttonGroup : Msg -> List (Html Msg)
buttonGroup action =
    [ p []
        [ button
            [ type_ "button"
            , onClick action
            ]
            []
        ]
    ]


buttonSong : SongGroup -> Int -> Html Msg
buttonSong group index =
    let
        groupString : String
        groupString =
            case group of
                Played ->
                    "Add"

                Remembered ->
                    "Drop"

        buttonId : String
        buttonId =
            "button" ++ groupString ++ toString index
    in
    button
        [ id buttonId
        , type_ "button"
        ]
        []


songView : Model -> SongGroup -> Int -> SongInfo -> Html Msg
songView model group index song =
    let
        amazonConstant : String
        amazonConstant =
            "http://www.amazon.com/s/ref=nb_sb_noss?tag=wtmdradio-20&url=search-alias%3Ddigital-music&field-keywords="

        buySong : List (Attribute msg)
        buySong =
            [ target "_blank"
            , href (amazonConstant ++ song.title ++ "+" ++ song.artist)
            ]

        lengthRemembered : Int
        lengthRemembered =
            List.length model.remembered

        songAttributes : List (Attribute msg)
        songAttributes =
            case model.shape of
                Expanded ->
                    []

                Shrunk ->
                    styleCalc group lengthRemembered index

        commentButton : Html Msg
        commentButton =
            case group of
                Played ->
                    text ""

                Remembered ->
                    buttonSong group index

        commentedIndicator : Html Msg
        commentedIndicator =
            case song.commented of
                False ->
                    text ""

                True ->
                    em [] []
    in
    div
        songAttributes
        [ p []
            [ buttonSong group index
            , span []
                [ text song.time ]
            , commentButton
            , commentedIndicator
            , a
                buySong
                []
            ]
        , p []
            [ text song.title ]
        , p []
            [ text song.artist ]
        ]


view : Model -> Html Msg
view model =
    main_ []
        [ section
            (groupAttributes Remembered)
            (buttonGroup Morph ++ songsOfGroup model Remembered)
        , hr [] []
        , section
            (groupAttributes Played)
            (buttonGroup Refresh ++ songsOfGroup model Played)
        ]
