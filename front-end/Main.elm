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

{-
   ( decodeString
   , int
     -- TODO: remove int
   , list
   , string
   )
-}

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
import Http
    exposing
        ( Error
        )
import Json.Decode
    exposing
        ( decodeString
        , field
        )
import Task
    exposing
        ( attempt
        , perform
        , succeed
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


type alias HoverString =
    String


type alias Messages =
    List String


type alias Model =
    { commentText : CommentText
    , messages : Messages -- TODO: Do we need messages?
    , pageExpanded : PageExpanded
    , songRememberedCommentingIndex : Maybe SongRememberedIndex
    , songsLatestFew : SongsList
    , songsRemembered : SongsList
    }


type alias PageExpanded =
    Bool


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


type alias SongInfoRaw =
    { artist : Artist
    , title : Title
    , time : Time
    , timeStamp : TimeStamp
    }


type alias SongLatestFewIndex =
    Int


type alias SongRememberedIndex =
    Int


type alias SongsList =
    List SongInfo


type alias SongsListRaw =
    { latestFive : List SongInfoRaw
    }


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


pageExpandedInit : PageExpanded
pageExpandedInit =
    False


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


init : ( Model, Cmd msg )
init =
    ( Model commentTextInit messagesInit pageExpandedInit songRememberedCommentingIndexInit songsRememberedInit songsLatestFewInit
    , Cmd.none
    )



-- UPDATE


type Msg
    = CommentAreaShow SongRememberedIndex
    | CommentInputCancel
    | CommentInputOk
    | CommentTextChangeCapture CommentText
    | FocusResult (Result Dom.Error ())
    | FocusSet Id
    | PageReshape
    | SongForget SongRememberedIndex
    | SongRemember SongLatestFewIndex
    | SongsLatestFewRefresh
    | SongsLatestFewResponse (Result Http.Error String)



-- https://www.reddit.com/r/elm/comments/53y6s4/focus_on_input_box_after_clicking_button/
-- https://stackoverflow.com/a/39419640/1136063


domFocus : Id -> Cmd Msg
domFocus id =
    Task.attempt FocusResult (Dom.focus id)


focusSet : Id -> Cmd Msg
focusSet id =
    msg2Cmd (Task.succeed (FocusSet id))



--For wrapping a message as a `Cmd`, see:
-- https://github.com/billstclair/elm-dynamodb/blob/7ac30d60b98fbe7ea253be13f5f9df4d9c661b92/src/DynamoBackend.elm


msg2Cmd : Task.Task Never msg -> Cmd msg
msg2Cmd msg =
    Task.perform identity msg



--For decoding Json, see:
--https://medium.com/@eeue56/json-decoding-in-elm-is-still-difficult-cad2d1fb39ae
--http://eeue56.github.io/json-to-elm/


decodeSongInfoRaw : Json.Decode.Decoder SongInfoRaw
decodeSongInfoRaw =
    Json.Decode.map4 SongInfoRaw
        (field "artist" Json.Decode.string)
        (field "title" Json.Decode.string)
        (field "time" Json.Decode.string)
        (field "timeStamp" Json.Decode.string)


decodeSongsListRaw : Json.Decode.Decoder SongsListRaw
decodeSongsListRaw =
    Json.Decode.map SongsListRaw
        (field "latestFive" (Json.Decode.list decodeSongInfoRaw))


songsLatestFewGet : List SongInfo
songsLatestFewGet =
    let
        stringJson : String
        stringJson =
            """
{ "latestFive":
  [
    { "artist": "The Herd Of Main Street",
      "title": "Never Look Back",
      "time": "4:54 PM",
      "timeStamp": "2017 08 29 16 54"
    },
    { "artist": "Susan Alcorn",
      "title": "Baltimore Hit Parade",
      "time": "3:55 PM",
      "timeStamp": "2017 08 29 15 55"
    },
    { "artist": "Thao and the Get Down Stay D",
      "title": "Astonished Man",
      "time": "3:51 PM",
      "timeStamp": "2017 08 29 15 51"
    },
    { "artist": "The Herd Of Main Street",
      "title": "Never Look Back",
      "time": "3:47 PM",
      "timeStamp": "2017 08 29 15 47"
    },
    { "artist": "Djembe Jones",
      "title": "Nowhere",
      "time": "3:42 PM",
      "timeStamp": "2017 08 29 15 42"
    }
  ]
}
"""

        raw : Result String SongsListRaw
        raw =
            decodeString decodeSongsListRaw stringJson

        rawUnpacked : List SongInfoRaw
        rawUnpacked =
            case raw of
                Err _ ->
                    []

                Ok json ->
                    json.latestFive

        addFields : SongInfoRaw -> SongInfo
        addFields songInfoRaw =
            { artist = songInfoRaw.artist
            , commented = False
            , time = songInfoRaw.time
            , timeStamp = songInfoRaw.timeStamp
            , title = songInfoRaw.title
            }
    in
    List.map addFields rawUnpacked


songsLatestFewRequest : Cmd Msg
songsLatestFewRequest =
    let
        request : Http.Request String
        request =
            Http.getString url

        -- songsLatestFewDecode
        url : String
        url =
            "https://wtmd.org/wtmdapp/LatestFiveHD2.json"
    in
    Http.send SongsLatestFewResponse request


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        focusInputPossibly : Cmd Msg
        focusInputPossibly =
            if Nothing == model.songRememberedCommentingIndex then
                Cmd.none
            else
                focusSet "input"
    in
    case msg of
        CommentAreaShow index ->
            let
                indexNew : SongRememberedIndex
                indexNew =
                    Maybe.withDefault index model.songRememberedCommentingIndex
            in
            ( { model
                | songRememberedCommentingIndex = Just indexNew
              }
            , focusSet "input"
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
                commands : Cmd Msg
                commands =
                    Cmd.batch [ focusInputPossibly ]

                showHasCommented : SongRememberedIndex -> SongInfo -> SongInfo
                showHasCommented index song =
                    if
                        String.isEmpty model.commentText
                            || (model.songRememberedCommentingIndex == Nothing)
                            || (model.songRememberedCommentingIndex /= Just index)
                    then
                        song
                    else
                        -- TODO: make AJAX request.
                        { song
                            | commented = True
                        }

                songRememberedCommentingIndexNew : Maybe SongRememberedIndex
                songRememberedCommentingIndexNew =
                    if "" /= model.commentText then
                        Nothing
                    else
                        model.songRememberedCommentingIndex

                songsRememberedNew : SongsList
                songsRememberedNew =
                    List.indexedMap showHasCommented model.songsRemembered
            in
            ( { model
                | commentText = ""
                , songRememberedCommentingIndex = songRememberedCommentingIndexNew
                , songsRemembered = songsRememberedNew
              }
            , commands
            )

        CommentTextChangeCapture text ->
            ( { model
                | commentText = text
              }
            , Cmd.none
            )

        FocusResult result ->
            ( model
            , Cmd.none
            )

        FocusSet id ->
            ( model
            , domFocus id
            )

        PageReshape ->
            ( { model
                | pageExpanded = not model.pageExpanded
              }
            , focusInputPossibly
            )

        SongForget index ->
            let
                focusId : Id
                focusId =
                    if Nothing == model.songRememberedCommentingIndex then
                        "refresh"
                    else
                        "input"

                songsRememberedNew : SongsList
                songsRememberedNew =
                    if Nothing == model.songRememberedCommentingIndex then
                        withoutOne
                    else
                        model.songsRemembered

                withoutOne : SongsList
                withoutOne =
                    List.take index model.songsRemembered
                        ++ List.drop (index + 1) model.songsRemembered
            in
            ( { model
                | songsRemembered = songsRememberedNew
              }
            , focusSet focusId
            )

        SongRemember index ->
            let
                songSelected : Maybe SongInfo
                songSelected =
                    List.head (List.drop index model.songsLatestFew)

                songsDifferent : SongsList
                songsDifferent =
                    if Nothing == songSelected then
                        model.songsRemembered
                    else
                        List.filter (\x -> Just x /= songSelected) model.songsRemembered

                songsRememberedNew : SongsList
                songsRememberedNew =
                    case songSelected of
                        Nothing ->
                            model.songsRemembered

                        Just songSelected ->
                            if List.member songSelected model.songsRemembered then
                                model.songsRemembered
                            else
                                songsDifferent ++ [ songSelected ]
            in
            ( { model
                | songsRemembered = songsRememberedNew
              }
            , focusInputPossibly
            )

        SongsLatestFewRefresh ->
            ( model
            , Cmd.batch [ focusInputPossibly, songsLatestFewRequest ]
            )

        SongsLatestFewResponse (Ok latestFew) ->
            let
                songsLatestFewNew : SongsList
                songsLatestFewNew =
                    songsLatestFewInitFull
            in
            ( { model
                | songsLatestFew = songsLatestFewNew
              }
            , Cmd.none
            )

        SongsLatestFewResponse (Err _) ->
            ( model
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


buttonComment : SongGroup -> SongRememberedIndex -> Html Msg
buttonComment group index =
    let
        action : Msg
        action =
            CommentAreaShow index

        buttonId : Maybe Id
        buttonId =
            Just ("buttonComment" ++ toString index)

        hoverString : HoverString
        hoverString =
            "Share a comment (with the DJ) about this song"
    in
    case group of
        Played ->
            text ""

        Remembered ->
            buttonMy buttonId hoverString action


buttonGroup : SongGroup -> List (Html Msg)
buttonGroup group =
    let
        action : Msg
        action =
            case group of
                Played ->
                    SongsLatestFewRefresh

                Remembered ->
                    PageReshape

        buttonId : Maybe Id
        buttonId =
            case group of
                Played ->
                    Just "refresh"

                Remembered ->
                    Just "morph"

        hoverString : HoverString
        hoverString =
            case group of
                Played ->
                    "Refresh the latest few songs"

                Remembered ->
                    "Morph this page's shape"
    in
    [ p []
        [ buttonMy buttonId hoverString action ]
    ]


buttonLike : SongGroup -> SongRememberedIndex -> Html Msg
buttonLike group index =
    let
        action : Msg
        action =
            CommentAreaShow index

        buttonId : Maybe Id
        buttonId =
            Just ("buttonLike" ++ toString index)

        hoverString : HoverString
        hoverString =
            "Share a 'Like' (with the DJ) about this song"
    in
    case group of
        Played ->
            text ""

        Remembered ->
            buttonMy buttonId hoverString action


buttonMy : Maybe Id -> HoverString -> Msg -> Html Msg
buttonMy buttonId hoverString action =
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
         , title hoverString
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

        buttonId : Maybe Id
        buttonId =
            Just ("button" ++ groupString ++ toString index)

        groupString : String
        groupString =
            case group of
                Played ->
                    "Remember"

                Remembered ->
                    "Forget"

        hoverString : HoverString
        hoverString =
            case group of
                Played ->
                    "Add this song (to remembered songs)"

                Remembered ->
                    "Drop this song (from remembered songs)"
    in
    buttonMy buttonId hoverString action


commentArea : Model -> Html Msg
commentArea model =
    let
        commentTextStatistics : String
        commentTextStatistics =
            -- ""
            ": " ++ toString (String.length model.commentText)

        prompt : String
        prompt =
            "Type your (additional) comment here!"

        song : SongRememberedIndex -> Maybe SongInfo
        song index =
            List.head (List.drop index model.songsRemembered)
    in
    case model.songRememberedCommentingIndex of
        Nothing ->
            text ""

        Just index ->
            case song index of
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
                                    ++ ") "
                                    ++ song.timeStamp
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
            if not song.commented then
                text ""
            else
                em [ title "You've left a comment about this song" ]
                    []

        lengthRemembered : SongGroupLength
        lengthRemembered =
            List.length model.songsRemembered

        songAttributes : List (Attribute msg)
        songAttributes =
            if model.pageExpanded then
                []
            else
                styleCalc group lengthRemembered index
    in
    div
        songAttributes
        [ p []
            [ buttonRememberForget group index
            , span []
                [ text song.time ]
            , buttonComment group index
            , buttonLike group index
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
