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

import Dom
    exposing
        ( Id
        , focus
        )
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
        )
import Task
    exposing
        ( attempt
        , perform
        , succeed
        )
import Tuple exposing (second)


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
    , songRememberedCommentingIndex : Maybe SongRememberedIndex
    , songsLatestFew : SongsList
    , songsRemembered : SongsList
    }


type alias SongGroupLength =
    Int


type alias SongIndex =
    Int


type alias SongInfo =
    { artist : Artist
    , commented : Commented
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongLatestFewIndex =
    Int


type alias SongRememberedIndex =
    Int


type alias SongsList =
    List SongInfo


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


songRememberedCommentingIndexInit : Maybe SongRememberedIndex
songRememberedCommentingIndexInit =
    Nothing


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
    ( Model commentTextInit messagesInit Shrunk songRememberedCommentingIndexInit songsRememberedInit songsLatestFewInit
    , Cmd.none
    )



-- UPDATE


type Msg
    = CommentAreaShow SongRememberedIndex
    | CommentInputCancel
    | CommentInputOk
    | CommentTextChangeCapture String
    | FocusResult (Result Dom.Error ())
    | FocusSet Id
    | PageShapeMorph
    | SongForget SongRememberedIndex
    | SongRemember SongLatestFewIndex
    | SongsLatestFewRefresh


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        focusInput : Cmd Msg
        focusInput =
            case model.songRememberedCommentingIndex of
                Nothing ->
                    Cmd.none

                _ ->
                    focusSetTask "input"

        -- For wrapping a message as a `Cmd`, see:
        -- https://github.com/billstclair/elm-dynamodb/blob/7ac30d60b98fbe7ea253be13f5f9df4d9c661b92/src/DynamoBackend.elm
        focusSetTask : Id -> Cmd Msg
        focusSetTask id =
            Task.perform identity (Task.succeed (FocusSet id))
    in
    case msg of
        CommentAreaShow index ->
            let
                indexNew : SongRememberedIndex
                indexNew =
                    case model.songRememberedCommentingIndex of
                        Just indexCommentingAlready ->
                            indexCommentingAlready

                        Nothing ->
                            index
            in
            ( { model
                | songRememberedCommentingIndex = Just indexNew
              }
            , focusSetTask "input"
            )

        CommentInputCancel ->
            ( { model
                | commentText = ""
                , songRememberedCommentingIndex = Nothing
              }
            , Cmd.none
            )

        CommentInputOk ->
            let
                displayHasCommented : SongRememberedIndex -> SongInfo -> SongInfo
                displayHasCommented index song =
                    case String.isEmpty model.commentText of
                        True ->
                            song

                        _ ->
                            case model.songRememberedCommentingIndex of
                                Nothing ->
                                    song

                                Just songRememberedCommentingIndex ->
                                    case songRememberedCommentingIndex == index of
                                        False ->
                                            song

                                        -- TODO: make AJAX request.
                                        _ ->
                                            { song
                                                | commented = True
                                            }

                songRememberedCommentingIndexNew : Maybe SongRememberedIndex
                songRememberedCommentingIndexNew =
                    case String.isEmpty model.commentText of
                        False ->
                            Nothing

                        _ ->
                            model.songRememberedCommentingIndex

                songsRememberedNew : SongsList
                songsRememberedNew =
                    List.indexedMap displayHasCommented model.songsRemembered
            in
            ( { model
                | commentText = ""
                , songRememberedCommentingIndex = songRememberedCommentingIndexNew
                , songsRemembered = songsRememberedNew
              }
            , focusInput
            )

        CommentTextChangeCapture text ->
            ( { model
                | commentText = text
              }
            , Cmd.none
            )

        -- https://www.reddit.com/r/elm/comments/53y6s4/focus_on_input_box_after_clicking_button/
        -- https://stackoverflow.com/a/39419640/1136063
        FocusSet id ->
            ( model
            , Task.attempt FocusResult (Dom.focus id)
            )

        FocusResult result ->
            ( model
            , Cmd.none
            )

        PageShapeMorph ->
            let
                pageShapeMorphed : PageShape
                pageShapeMorphed =
                    case model.pageShape of
                        Expanded ->
                            Shrunk

                        Shrunk ->
                            Expanded
            in
            ( { model
                | pageShape = pageShapeMorphed
              }
            , focusInput
            )

        SongForget index ->
            let
                focus : Cmd Msg
                focus =
                    case model.songRememberedCommentingIndex of
                        Nothing ->
                            focusSetTask "refresh"

                        _ ->
                            focusSetTask "input"

                songsRememberedNew : SongsList
                songsRememberedNew =
                    case model.songRememberedCommentingIndex of
                        Nothing ->
                            withoutOne

                        _ ->
                            model.songsRemembered

                withoutOne : SongsList
                withoutOne =
                    List.take index model.songsRemembered
                        ++ List.drop (index + 1) model.songsRemembered
            in
            ( { model
                | songsRemembered = songsRememberedNew
              }
            , focus
            )

        SongRemember index ->
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
            ( { model
                | songsRemembered = songsRememberedNew
              }
            , focusInput
            )

        SongsLatestFewRefresh ->
            ( { model
                | songsLatestFew = songsLatestFewInitFull
                , songsRemembered = songsRememberedInitFull
              }
            , focusInput
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


buttonComment : SongGroup -> SongRememberedIndex -> Html Msg
buttonComment group index =
    let
        action : Msg
        action =
            CommentAreaShow index

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
                    SongsLatestFewRefresh

                Remembered ->
                    PageShapeMorph

        buttonId : Maybe String
        buttonId =
            case group of
                Played ->
                    Just "refresh"

                Remembered ->
                    Just "morph"

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
        ([ onClick action
         , title titleString
         , type_ "button"
         ]
            ++ idMy
        )
        []


buttonRememberForget : SongGroup -> SongIndex -> Html Msg
buttonRememberForget group index =
    let
        action : Msg
        action =
            case group of
                Played ->
                    SongRemember index

                Remembered ->
                    SongForget index

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
    case model.songRememberedCommentingIndex of
        Nothing ->
            text ""

        Just index ->
            let
                commentTextStatistics : String
                commentTextStatistics =
                    -- ""
                    " " ++ toString (String.length model.commentText)

                prompt : String
                prompt =
                    "Type your comment here!"

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
                            [ autocomplete False
                            , id "input"
                            , onInput CommentTextChangeCapture
                            , placeholder prompt
                            , required True
                            , title prompt
                            , type_ "text"
                            ]
                            []
                        , buttonMy Nothing "Submit your comment" CommentInputOk
                        , buttonMy Nothing "Cancel this comment" CommentInputCancel
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
    [ class "songs-group"
    , id ("songs-" ++ groupString)
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


songView : Model -> SongGroup -> SongIndex -> SongInfo -> Html Msg
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
            [ href (amazonConstant ++ song.title ++ "+" ++ song.artist)
            , target "_blank"
            , title "See song on Amazon (in new tab)"
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


styleCalc : SongGroup -> SongGroupLength -> SongIndex -> List (Attribute msg)
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

        indexReversed : SongIndex
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
