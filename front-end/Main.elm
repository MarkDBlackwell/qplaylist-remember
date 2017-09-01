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

import Debug exposing (log)
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
        , Request
        , getString
        , send
        )
import Json.Decode
    exposing
        ( Decoder
        , decodeString
        , field
        , list
        , map
        , map4
        , string
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


type alias LikeOrCommentText =
    String


type alias Model =
    { likeOrCommentText : LikeOrCommentText
    , pageExpanded : PageIsExpanded
    , songRememberedCommentingIndex : Maybe SongRememberedIndex
    , songsLatestFew : SongsList
    , songsRemembered : SongsList
    }


type alias PageIsExpanded =
    Bool


type alias SongRememberedIndex =
    Int


type alias SongsList =
    List SongInfo


likeOrCommentTextInit : LikeOrCommentText
likeOrCommentTextInit =
    ""


pageExpandedInit : PageIsExpanded
pageExpandedInit =
    False


songRememberedCommentingIndexInit : Maybe SongRememberedIndex
songRememberedCommentingIndexInit =
    Nothing


songsLatestFewInit : SongsList
songsLatestFewInit =
    []


songsRememberedInit : SongsList
songsRememberedInit =
    []


init : ( Model, Cmd msg )
init =
    ( Model likeOrCommentTextInit pageExpandedInit songRememberedCommentingIndexInit songsRememberedInit songsLatestFewInit
    , Cmd.none
    )



-- UPDATE


type alias LikedOrCommented =
    Bool


type alias DecodeErrorMessageText =
    String


type alias HttpErrorMessageText =
    String


type alias HttpRequestText =
    String


type alias HttpResponseText =
    String


type alias SongInfo =
    { artist : Artist
    , likedOrCommented : LikedOrCommented
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongInfoRaw =
    --Keep order:
    { artist : Artist
    , title : Title
    , time : Time
    , timeStamp : TimeStamp
    }


type alias SongLatestFewIndex =
    Int


type alias SongsListRaw =
    { latestFive : List SongInfoRaw }


type alias Time =
    String


type alias TimeStamp =
    String


type alias Title =
    String


type alias UrlText =
    String


type Msg
    = CommentAreaShow SongRememberedIndex
    | CommentInputCancel
    | CommentInputOk
    | CommentTextChangeCapture LikeOrCommentText
    | FocusResult (Result Dom.Error ())
    | FocusSet Id
    | LikeProcess SongRememberedIndex
    | PageReshape
    | SongForget SongRememberedIndex
    | SongRemember SongLatestFewIndex
    | SongsLatestFewRefresh
    | SongsLatestFewResponse (Result Error HttpResponseText)


decodeSongRaw : Decoder SongInfoRaw
decodeSongRaw =
    --For decoding Json, see:
    --https://medium.com/@eeue56/json-decoding-in-elm-is-still-difficult-cad2d1fb39ae
    --http://eeue56.github.io/json-to-elm/
    map4 SongInfoRaw
        (field "artist" string)
        (field "title" string)
        (field "time" string)
        (field "timeStamp" string)


decodeSongsLatestFew : HttpResponseText -> List SongInfo
decodeSongsLatestFew stringJson =
    let
        addFields : SongInfoRaw -> SongInfo
        addFields songInfoRaw =
            { artist = songInfoRaw.artist
            , likedOrCommented = False
            , time = songInfoRaw.time
            , timeStamp = songInfoRaw.timeStamp
            , title = songInfoRaw.title
            }

        raw : Result DecodeErrorMessageText SongsListRaw
        raw =
            decodeString decodeSongsLatestFewRaw stringJson

        rawUnpacked : List SongInfoRaw
        rawUnpacked =
            case raw of
                Err _ ->
                    []

                Ok json ->
                    json.latestFive
    in
    List.map addFields rawUnpacked


decodeSongsLatestFewRaw : Decoder SongsListRaw
decodeSongsLatestFewRaw =
    map SongsListRaw
        (field "latestFive" (list decodeSongRaw))


focusSet : Id -> Cmd Msg
focusSet id =
    msg2Cmd (succeed (FocusSet id))


msg2Cmd : Task.Task Never msg -> Cmd msg
msg2Cmd msg =
    --For wrapping a message as a `Cmd`, see:
    -- https://github.com/billstclair/elm-dynamodb/blob/7ac30d60b98fbe7ea253be13f5f9df4d9c661b92/src/DynamoBackend.elm
    Task.perform identity msg


songsLatestFewRequest : Cmd Msg
songsLatestFewRequest =
    let
        basename : UrlText
        basename =
            "LatestFive.json"

        request : Request HttpRequestText
        request =
            getString
                (subUri
                    ++ basename
                )

        subUri : UrlText
        subUri =
            "/wtmdapp/"
    in
    send SongsLatestFewResponse request


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        focusInputPossibly : Cmd Msg
        focusInputPossibly =
            if model.songRememberedCommentingIndex == songRememberedCommentingIndexInit then
                Cmd.none
            else
                focusSet "input"

        httpErrorMessageText : Error -> HttpErrorMessageText
        httpErrorMessageText httpError =
            let
                prefix : HttpErrorMessageText
                prefix =
                    "HttpError"
            in
            case httpError of
                Http.BadPayload debuggingText httpResponseText ->
                    log (prefix ++ ": BadPayload") debuggingText

                Http.BadStatus httpResponseText ->
                    log prefix "BadStatus"

                Http.BadUrl urlText ->
                    log (prefix ++ ": BadUrl") urlText

                Http.NetworkError ->
                    log prefix "NetworkError"

                Http.Timeout ->
                    log prefix "Timeout"
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
                | likeOrCommentText = likeOrCommentTextInit
                , songRememberedCommentingIndex = songRememberedCommentingIndexInit
              }
            , Cmd.none
            )

        CommentInputOk ->
            let
                commentedShow : SongRememberedIndex -> SongInfo -> SongInfo
                commentedShow index song =
                    if Just index == model.songRememberedCommentingIndex then
                        { song
                            | likedOrCommented = True
                        }
                    else
                        song

                songsRememberedNew : SongsList
                songsRememberedNew =
                    List.indexedMap commentedShow model.songsRemembered
            in
            if String.isEmpty model.likeOrCommentText then
                ( model
                , focusInputPossibly
                )
            else
                --TODO: make AJAX request.
                ( { model
                    | likeOrCommentText = likeOrCommentTextInit
                    , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                    , songsRemembered = songsRememberedNew
                  }
                , Cmd.none
                )

        CommentTextChangeCapture text ->
            ( { model
                | likeOrCommentText = text
              }
            , Cmd.none
            )

        FocusResult result ->
            ( model
            , Cmd.none
            )

        FocusSet id ->
            --https://www.reddit.com/r/elm/comments/53y6s4/focus_on_input_box_after_clicking_button/
            --https://stackoverflow.com/a/39419640/1136063
            ( model
            , attempt FocusResult (focus id)
            )

        LikeProcess songRememberedIndex ->
            let
                likeText : LikeOrCommentText
                likeText =
                    "Loved it!"

                likedShow : SongRememberedIndex -> SongInfo -> SongInfo
                likedShow index song =
                    if index == songRememberedIndex then
                        { song
                            | likedOrCommented = True
                        }
                    else
                        song

                songsRememberedNew : SongsList
                songsRememberedNew =
                    List.indexedMap likedShow model.songsRemembered
            in
            case model.songRememberedCommentingIndex of
                Just _ ->
                    ( model
                    , Cmd.none
                    )

                songRememberedCommentingIndexInit ->
                    ( { model
                        | likeOrCommentText = likeText
                        , songRememberedCommentingIndex = Just songRememberedIndex
                        , songsRemembered = songsRememberedNew
                      }
                    , msg2Cmd (succeed CommentInputOk)
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
                    if model.songRememberedCommentingIndex == songRememberedCommentingIndexInit then
                        "refresh"
                    else
                        "input"

                songsRememberedNew : SongsList
                songsRememberedNew =
                    if model.songRememberedCommentingIndex == songRememberedCommentingIndexInit then
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

        SongRemember songLatestFewIndex ->
            let
                songClean : SongInfo -> SongInfo
                songClean song =
                    { song | likedOrCommented = False }

                songDiffers : SongInfo -> Bool
                songDiffers song =
                    case songSelected of
                        Nothing ->
                            True

                        Just songSelected ->
                            songClean songSelected /= songClean song

                songSelected : Maybe SongInfo
                songSelected =
                    List.head (List.drop songLatestFewIndex model.songsLatestFew)

                songsDifferent : SongsList
                songsDifferent =
                    if Nothing == songSelected then
                        model.songsRemembered
                    else
                        List.filter songDiffers model.songsRemembered

                songsRememberedCleaned : SongsList
                songsRememberedCleaned =
                    List.map songClean model.songsRemembered

                songsRememberedNew : SongsList
                songsRememberedNew =
                    case songSelected of
                        Nothing ->
                            model.songsRemembered

                        Just songSelected ->
                            if List.member (songClean songSelected) songsRememberedCleaned then
                                model.songsRemembered
                            else
                                songsDifferent
                                    ++ [ songSelected ]
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

        SongsLatestFewResponse (Err httpError) ->
            let
                --Keep for console logging:
                songsLatestFewResponseHttpErrorMessageText : HttpErrorMessageText
                songsLatestFewResponseHttpErrorMessageText =
                    httpErrorMessageText httpError
            in
            ( model
            , Cmd.none
            )

        SongsLatestFewResponse (Ok songsLatestFewJson) ->
            let
                songsLatestFewNew : SongsList
                songsLatestFewNew =
                    decodeSongsLatestFew songsLatestFewJson
            in
            ( { model
                | songsLatestFew = songsLatestFewNew
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- VIEW


type alias HoverText =
    String


type alias SongGroupLength =
    Int


type alias SongIndex =
    Int


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
            Just
                ("buttonComment"
                    ++ toString index
                )

        hoverText : HoverText
        hoverText =
            "Share a comment (with the DJ) about this song"
    in
    if Remembered == group then
        buttonMy buttonId hoverText action
    else
        htmlNodeNull


buttonForgetRemember : SongGroup -> SongIndex -> Html Msg
buttonForgetRemember group index =
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
            Just
                ("button"
                    ++ groupString
                    ++ toString index
                )

        groupString : String
        groupString =
            case group of
                Played ->
                    "Remember"

                Remembered ->
                    "Forget"

        hoverText : HoverText
        hoverText =
            case group of
                Played ->
                    "Add this song (to remembered songs)"

                Remembered ->
                    "Drop this song (from remembered songs)"
    in
    buttonMy buttonId hoverText action


buttonLike : SongGroup -> SongRememberedIndex -> Html Msg
buttonLike group index =
    let
        action : Msg
        action =
            LikeProcess index

        buttonId : Maybe Id
        buttonId =
            Just
                ("buttonLike"
                    ++ toString index
                )

        hoverText : HoverText
        hoverText =
            "Share a 'Like' (with the DJ) about this song"
    in
    case group of
        Played ->
            htmlNodeNull

        Remembered ->
            buttonMy buttonId hoverText action


buttonMy : Maybe Id -> HoverText -> Msg -> Html Msg
buttonMy buttonId hoverText action =
    let
        display : String
        display =
            case buttonId of
                Nothing ->
                    "inline-block"

                Just buttonId ->
                    if
                        String.startsWith "buttonComment" buttonId
                            && not showCommentButtons
                    then
                        "none"
                    else
                        "inline-block"

        idMy : List (Attribute msg)
        idMy =
            case buttonId of
                Nothing ->
                    []

                Just buttonId ->
                    [ id buttonId ]
    in
    button
        ([ style [ ( "display", display ) ]
         , onClick action
         , title hoverText
         , type_ "button"
         ]
            ++ idMy
        )
        []


buttonPlayed : Html Msg
buttonPlayed =
    let
        buttonId : Maybe Id
        buttonId =
            Just "refresh"

        hoverText : HoverText
        hoverText =
            "Refresh the latest few songs"
    in
    buttonMy buttonId hoverText SongsLatestFewRefresh


buttonRemembered : Html Msg
buttonRemembered =
    let
        buttonId : Maybe Id
        buttonId =
            Just "morph"

        hoverText : HoverText
        hoverText =
            "Morph this page's shape"
    in
    buttonMy buttonId hoverText PageReshape


commentArea : Model -> SongInfo -> Html Msg
commentArea model song =
    let
        commentTextStatistics : String
        commentTextStatistics =
            " – "
                ++ song.timeStamp
                ++ ": "
                ++ toString (String.length model.likeOrCommentText)

        hoverText : HoverText
        hoverText =
            "Type your (additional) comment here!"
    in
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
            , placeholder hoverText
            , required True
            , title hoverText
            , type_ "text"
            ]
            []
        , buttonMy Nothing "Submit your comment" CommentInputOk
        , buttonMy Nothing "Cancel this comment" CommentInputCancel
        ]


commentAreaPossibly : Model -> Html Msg
commentAreaPossibly model =
    let
        songPossibly : SongRememberedIndex -> Maybe SongInfo
        songPossibly index =
            List.head (List.drop index model.songsRemembered)
    in
    case model.songRememberedCommentingIndex of
        Just index ->
            case songPossibly index of
                Nothing ->
                    htmlNodeNull

                Just song ->
                    commentArea model song

        songRememberedCommentingIndexInit ->
            htmlNodeNull


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
    , id
        ("songs-"
            ++ groupString
        )
    ]


htmlNodeNull : Html Msg
htmlNodeNull =
    text ""


showCommentButtons : Bool
showCommentButtons =
    True


songView : Model -> SongGroup -> SongIndex -> SongInfo -> Html Msg
songView model group index song =
    let
        amazonConstant : String
        amazonConstant =
            --%3D represents the "equals" sign:
            "http://www.amazon.com/s/ref=nb_sb_noss?"
                ++ "tag=wtmdradio-20"
                ++ "&url=search-alias%3Ddigital-music"
                ++ "&field-keywords="

        anchorBuySongAttributes : List (Attribute msg)
        anchorBuySongAttributes =
            [ href
                (amazonConstant
                    ++ song.title
                    ++ "+"
                    ++ song.artist
                )
            , target "_blank"
            , title buySong
            ]

        buySong : String
        buySong =
            "See this song on Amazon (in new tab)"

        lengthRemembered : SongGroupLength
        lengthRemembered =
            List.length model.songsRemembered

        likedOrCommentedIndicator : Html Msg
        likedOrCommentedIndicator =
            if song.likedOrCommented then
                em [ title likedOrCommentedIndicatorHoverText ]
                    []
            else
                htmlNodeNull

        likedOrCommentedIndicatorHoverText : HoverText
        likedOrCommentedIndicatorHoverText =
            "You've shared a 'Like'"
                ++ likedOrCommentedIndicatorHoverTextCommentButton
                ++ " about this song (with the DJ)"

        likedOrCommentedIndicatorHoverTextCommentButton : HoverText
        likedOrCommentedIndicatorHoverTextCommentButton =
            if showCommentButtons then
                " (or a comment)"
            else
                ""

        songAttributes : List (Attribute msg)
        songAttributes =
            if model.pageExpanded then
                []
            else
                [ styleCalc group lengthRemembered index ]
    in
    div
        songAttributes
        [ p []
            [ buttonForgetRemember group index
            , span []
                [ text song.time ]
            , buttonComment group index
            , buttonLike group index
            , likedOrCommentedIndicator
            , a
                anchorBuySongAttributes
                []
            ]
        , p []
            [ text song.title ]
        , p []
            [ text song.artist ]
        ]


styleCalc : SongGroup -> SongGroupLength -> SongIndex -> Attribute msg
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
            toString (sizeFactor * base)
                ++ "px"

        goldenRatio : Float
        goldenRatio =
            --Golden ratio:
            --https://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344
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
    style
        (backgroundColorStyling
            ++ fontSizeStyling
        )


view : Model -> Html Msg
view model =
    let
        songsLatestFew : List (Html Msg)
        songsLatestFew =
            List.indexedMap (songView model Played) model.songsLatestFew

        songsRemembered : List (Html Msg)
        songsRemembered =
            List.indexedMap (songView model Remembered) model.songsRemembered
    in
    main_
        []
        [ commentAreaPossibly model
        , section
            (groupAttributes Remembered)
            ([ p []
                [ buttonRemembered ]
             ]
                ++ songsRemembered
            )
        , hr [] []
        , section
            (groupAttributes Played)
            ([ p []
                [ buttonPlayed ]
             ]
                ++ songsLatestFew
            )
        ]
