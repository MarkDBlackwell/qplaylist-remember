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
        , input
        , main_
        , p
        , section
        , span
        , text
        )
import Html.Attributes
    exposing
        ( autocomplete
        , autofocus
        , class
        , href
        , id
        , placeholder
        , required
        , style
        , target
        , title
        , type_
        )
import Html.Events
    exposing
        ( onClick
        , onInput
        , onSubmit
        )
import Tuple
    exposing
        ( second
        )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias Artist =
    String


type alias CommentText =
    String


type alias Commented =
    Bool


type alias Messages =
    List String


type alias Model =
    { commentText : CommentText
    , messages : Messages -- TODO: Do we need messages?
    , pageShape : PageShape
    , songsLatestFew : SongsList
    , songsRemembered : SongsList
    , songsRememberedCommentingIndex : Maybe SongsRememberedIndex
    }


type alias SongGroupLength =
    Int


type alias SongInfo =
    { artist : Artist
    , commented : Commented
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongsLatestFewIndex =
    Int


type alias SongsLatestFewOrRememberedIndex =
    Int


type alias SongsList =
    List SongInfo


type alias SongsRememberedIndex =
    Int


type alias Time =
    String


type alias TimeStamp =
    String


type alias Title =
    String


commentTextInit : CommentText
commentTextInit =
    ""


messagesInit : Messages
messagesInit =
    []


songInfo : Artist -> Title -> Time -> TimeStamp -> Commented -> SongInfo
songInfo artist title time timeStamp commented =
    { artist = artist
    , commented = commented
    , time = time
    , timeStamp = timeStamp
    , title = title
    }


songsLatestFewInit : SongsList
songsLatestFewInit =
    []


songsLatestFewInitFull : SongsList
songsLatestFewInitFull =
    [ songInfo "U2"
        "Bullet The Blue Sky"
        "5:53 PM"
        "2017 08 07 17 53"
        False
    , songInfo "LP"
        "No Witness"
        "5:49 PM"
        "2017 08 07 17 49"
        False
    , songInfo "Cage The Elephant"
        "Whole Wide World"
        "5:46 PM"
        "2017 08 07 17 46"
        False
    , songInfo "Robert Randolph and the Fami"
        "Deliver Me"
        "5:41 PM"
        "2017 08 07 17 41"
        False
    , songInfo "Outer Spaces"
        "Words"
        "5:31 PM"
        "2017 08 07 17 31"
        False
    ]


songsRememberedCommentingIndexInit : Maybe SongsRememberedIndex
songsRememberedCommentingIndexInit =
    Nothing


songsRememberedInit : SongsList
songsRememberedInit =
    []


songsRememberedInitFull : SongsList
songsRememberedInitFull =
    [ songInfo "The Rosebuds"
        "In My Teeth"
        "4:54 PM"
        "2017 08 07 16 54"
        False
    , songInfo "T. Rex"
        "King Of The Rumbling Spires"
        "4:59 PM"
        "2017 08 07 16 59"
        False
    , songInfo "Tedeschi Trucks Band"
        "I Pity The Fool - Live"
        "5:07 PM"
        "2017 08 07 17 07"
        False
    , songInfo "Bobby \"Blue\" Bland"
        "I Pity The Fool"
        "5:14 PM"
        "2017 08 07 17 14"
        False
    , songInfo "Eddy Clearwater"
        "Find You A Job"
        "5:19 PM"
        "2017 08 07 17 19"
        False
    ]


init : ( Model, Cmd pageShape )
init =
    ( Model commentTextInit messagesInit Shrunk songsLatestFewInit songsRememberedInit songsRememberedCommentingIndexInit
    , Cmd.none
    )



-- UPDATE


type Msg
    = CommentCapture String
    | CommentOpen SongsRememberedIndex
    | Forget SongsRememberedIndex
    | InputCancel
    | InputOk
    | Morph
    | Refresh
    | Remember SongsLatestFewIndex


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        pageShapeMorphed : PageShape
        pageShapeMorphed =
            case model.pageShape of
                Expanded ->
                    Shrunk

                Shrunk ->
                    Expanded
    in
    case msg of
        CommentCapture commentText ->
            ( { model | commentText = commentText }
            , Cmd.none
            )

        CommentOpen index ->
            let
                songsRememberedCommentingIndexNew : SongsRememberedIndex
                songsRememberedCommentingIndexNew =
                    case model.songsRememberedCommentingIndex of
                        Nothing ->
                            index

                        Just a ->
                            a
            in
            ( { model | songsRememberedCommentingIndex = Just songsRememberedCommentingIndexNew }
            , Cmd.none
            )

        Forget index ->
            let
                withoutOne : SongsList
                withoutOne =
                    List.take index model.songsRemembered
                        ++ List.drop (index + 1) model.songsRemembered
            in
            ( { model | songsRemembered = withoutOne }
            , Cmd.none
            )

        InputCancel ->
            ( { model
                | commentText = ""
                , songsRememberedCommentingIndex = Nothing
              }
            , Cmd.none
            )

        InputOk ->
            let
                displayHasCommented : SongsRememberedIndex -> SongInfo -> SongInfo
                displayHasCommented index song =
                    case String.isEmpty model.commentText of
                        True ->
                            song

                        False ->
                            case model.songsRememberedCommentingIndex of
                                Nothing ->
                                    song

                                Just songsRememberedCommentingIndex ->
                                    case songsRememberedCommentingIndex == index of
                                        False ->
                                            song

                                        -- TODO: make AJAX request.
                                        True ->
                                            { song | commented = True }

                songsRememberedNew : SongsList
                songsRememberedNew =
                    List.indexedMap displayHasCommented model.songsRemembered
            in
            ( { model
                | commentText = ""
                , songsRemembered = songsRememberedNew
                , songsRememberedCommentingIndex = Nothing
              }
            , Cmd.none
            )

        Morph ->
            ( { model | pageShape = pageShapeMorphed }
            , Cmd.none
            )

        Refresh ->
            ( { model
                | songsLatestFew = songsLatestFewInitFull
                , songsRemembered = songsRememberedInitFull
              }
            , Cmd.none
            )

        Remember index ->
            let
                songSelected : Maybe SongInfo
                songSelected =
                    List.head (List.drop index model.songsLatestFew)

                songsDifferent : SongsList
                songsDifferent =
                    case songSelected of
                        Nothing ->
                            model.songsRemembered

                        Just songSelected ->
                            let
                                partition : ( SongsList, SongsList )
                                partition =
                                    List.partition (\x -> x == songSelected) model.songsRemembered
                            in
                            Tuple.second partition

                songsRememberedNew : SongsList
                songsRememberedNew =
                    case songSelected of
                        Nothing ->
                            model.songsRemembered

                        Just songSelected ->
                            case List.member songSelected model.songsRemembered of
                                True ->
                                    model.songsRemembered

                                False ->
                                    songsDifferent ++ [ songSelected ]
            in
            ( { model | songsRemembered = songsRememberedNew }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- VIEW


type PageShape
    = Expanded
    | Shrunk


type SongGroup
    = Played
    | Remembered


buttonComment : SongGroup -> SongsRememberedIndex -> Html Msg
buttonComment group index =
    let
        action : Msg
        action =
            CommentOpen index

        buttonId : Maybe String
        buttonId =
            Just ("buttonComment" ++ toString index)

        titleString : String
        titleString =
            "Share a comment (with the DJs) about this song"
    in
    case group of
        Played ->
            text ""

        Remembered ->
            buttonMy buttonId titleString action


buttonGroup : SongGroup -> List (Html Msg)
buttonGroup group =
    let
        action : Msg
        action =
            case group of
                Played ->
                    Refresh

                Remembered ->
                    Morph

        buttonId : Maybe String
        buttonId =
            Nothing

        titleString : String
        titleString =
            case group of
                Played ->
                    "Refresh the latest few songs"

                Remembered ->
                    "Morph this page's shape"
    in
    [ p []
        [ buttonMy buttonId titleString action ]
    ]


buttonMy : Maybe String -> String -> Msg -> Html Msg
buttonMy buttonId titleString action =
    let
        idMy : List (Attribute msg)
        idMy =
            case buttonId of
                Nothing ->
                    []

                Just buttonId ->
                    [ id buttonId ]
    in
    button
        ([ type_ "button"
         , title titleString
         , onClick action
         ]
            ++ idMy
        )
        []


buttonRememberForget : SongGroup -> SongsLatestFewOrRememberedIndex -> Html Msg
buttonRememberForget group index =
    let
        action : Msg
        action =
            case group of
                Played ->
                    Remember index

                Remembered ->
                    Forget index

        buttonId : Maybe String
        buttonId =
            Just ("button" ++ groupString ++ toString index)

        groupString : String
        groupString =
            case group of
                Played ->
                    "Remember"

                Remembered ->
                    "Forget"

        titleString : String
        titleString =
            case group of
                Played ->
                    "Add (to remembered songs)"

                Remembered ->
                    "Drop (from remembered songs)"
    in
    buttonMy buttonId titleString action


commentArea : Model -> Html Msg
commentArea model =
    case model.songsRememberedCommentingIndex of
        Nothing ->
            text ""

        Just index ->
            let
                commentTextStatistics : String
                commentTextStatistics =
                    -- " " ++ toString (String.length model.commentText)
                    ""

                song : Maybe SongInfo
                song =
                    List.head (List.drop index model.songsRemembered)
            in
            case song of
                Nothing ->
                    text ""

                Just song ->
                    section
                        [ id "comment" ]
                        [ p []
                            [ text
                                (song.artist
                                    ++ ": "
                                    ++ song.title
                                    ++ " ("
                                    ++ song.time
                                    ++ ")"
                                    ++ commentTextStatistics
                                )
                            ]
                        , input
                            [ id "input"
                            , type_ "text"
                            , onInput CommentCapture
                            , placeholder "Type your comment here!"
                            , autocomplete False
                            , autofocus True
                            , required True
                            ]
                            []
                        , buttonMy Nothing "Submit your comment" InputOk
                        , buttonMy Nothing "Cancel your comment" InputCancel
                        ]


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
                    model.songsLatestFew

                Remembered ->
                    model.songsRemembered
    in
    List.indexedMap (songView model group) songs


songView : Model -> SongGroup -> SongsLatestFewOrRememberedIndex -> SongInfo -> Html Msg
songView model group index song =
    let
        amazonConstant : String
        -- %3D represents the "equals" sign.
        amazonConstant =
            "http://www.amazon.com/s/ref=nb_sb_noss?"
                ++ "tag=wtmdradio-20"
                ++ "&url=search-alias%3Ddigital-music"
                ++ "&field-keywords="

        buySong : List (Attribute msg)
        buySong =
            [ target "_blank"
            , title "See song on Amazon (in new tab)"
            , href (amazonConstant ++ song.title ++ "+" ++ song.artist)
            ]

        commentedIndicator : Html Msg
        commentedIndicator =
            case song.commented of
                False ->
                    text ""

                True ->
                    em [ title "You've left a comment about this song" ]
                        []

        lengthRemembered : SongGroupLength
        lengthRemembered =
            List.length model.songsRemembered

        songAttributes : List (Attribute msg)
        songAttributes =
            case model.pageShape of
                Expanded ->
                    []

                Shrunk ->
                    styleCalc group lengthRemembered index
    in
    div
        songAttributes
        [ p []
            [ buttonRememberForget group index
            , span []
                [ text song.time ]
            , buttonComment group index
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


styleCalc : SongGroup -> SongGroupLength -> SongsLatestFewOrRememberedIndex -> List (Attribute msg)
styleCalc group songGroupLength index =
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
            toString (sizeFactor * base) ++ "px"

        -- Golden ratio:
        -- https://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344
        goldenRatio : Float
        goldenRatio =
            0.6180339887498949

        indexReversed : SongsLatestFewOrRememberedIndex
        indexReversed =
            songGroupLength - index - 1

        saturation : Float
        saturation =
            sizeFactor * 0.5

        sizeFactor : Float
        sizeFactor =
            case group of
                Played ->
                    goldenRatio ^ toFloat index

                Remembered ->
                    goldenRatio ^ toFloat indexReversed
    in
    [ style (backgroundColorStyling ++ fontSizeStyling) ]


view : Model -> Html Msg
view model =
    main_
        []
        [ commentArea model
        , section
            (groupAttributes Remembered)
            (buttonGroup Remembered ++ songsOfGroup model Remembered)
        , hr [] []
        , section
            (groupAttributes Played)
            (buttonGroup Played ++ songsOfGroup model Played)
        ]
