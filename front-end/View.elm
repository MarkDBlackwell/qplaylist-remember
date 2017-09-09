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


module View exposing (..)

import Dom
    exposing
        ( Id
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
import ModelDetails exposing (..)
import ModelDetailsUpdate exposing (..)
import ModelDetailsView exposing (..)
import MsgDetails exposing (..)


-- VIEW


showCommentButtons : Bool
showCommentButtons =
    --Keep above other functions.
    --TODO: Move to model.
    True


buttonComment : SongGroup -> SongRememberedIndex -> Html Msg
buttonComment group index =
    let
        buttonAction : Msg
        buttonAction =
            CommentInputSetUp index

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
        buttonMy buttonId hoverText buttonAction
    else
        htmlNodeNull


buttonForgetRemember : SongGroup -> SongIndex -> Html Msg
buttonForgetRemember group index =
    let
        buttonAction : Msg
        buttonAction =
            case group of
                Played ->
                    SongRemember index

                Remembered ->
                    SongForget index

        buttonId : Maybe Id
        buttonId =
            Just
                ("button"
                    ++ songGroup2String group
                    ++ toString index
                )

        hoverText : HoverText
        hoverText =
            case group of
                Played ->
                    "Add this song (to remembered songs)"

                Remembered ->
                    "Drop this song (from remembered songs)"
    in
    buttonMy buttonId hoverText buttonAction


buttonLike : SongGroup -> SongRememberedIndex -> Html Msg
buttonLike group index =
    let
        buttonAction : Msg
        buttonAction =
            LikeButtonProcess index

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
            buttonMy buttonId hoverText buttonAction


buttonMy : Maybe Id -> HoverText -> Msg -> Html Msg
buttonMy buttonId hoverText action =
    let
        display : Display
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
    buttonMy buttonId hoverText PageMorph


buySongAnchor : SongRemembered -> Html Msg
buySongAnchor song =
    let
        fieldKeywords : UriText
        fieldKeywords =
            String.join
                "+"
                [ song.title
                , song.artist
                ]

        hoverText : HoverText
        hoverText =
            "See this song on Amazon (in new tab)"

        queryBeforeList : QueryBeforeList
        queryBeforeList =
            [ "http://www.amazon.com/s/ref=nb_sb_noss" ]

        queryPairs : QueryPairs
        queryPairs =
            [ ( "tag", "wtmdradio-20" )
            , ( "url", "search-alias=digital-music" )
            , ( "field-keywords"
              , fieldKeywords
              )
            ]

        uriText : UriText
        uriText =
            relative queryBeforeList queryPairs
    in
    a
        [ href uriText
        , onClick BuySongAnchorProcess
        , target "_blank"
        , title hoverText
        ]
        []


commentArea : Model -> SongRemembered -> Html Msg
commentArea model song =
    let
        statistics : String
        statistics =
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
                    ++ statistics
                )
            ]
        , input
            [ autocomplete False
            , id "input"
            , onInput CommentInputTextChangeCapture
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
        songPossibly : SongRememberedIndex -> Maybe SongRemembered
        songPossibly index =
            List.head (List.drop index model.songsRemembered)
    in
    if not model.processingComment then
        htmlNodeNull
    else
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
    [ class "songs-group"
    , id
        ("songs-"
            ++ songGroup2String group
        )
    ]


htmlNodeNull : Html Msg
htmlNodeNull =
    text ""


likeOrCommentRequestUriText : Model -> UriText
likeOrCommentRequestUriText model =
    let
        artistTimeTitle : UriText
        artistTimeTitle =
            case index of
                Nothing ->
                    ""

                Just _ ->
                    case songSelected of
                        Nothing ->
                            ""

                        Just songSelected ->
                            songSelected.time
                                ++ " "
                                ++ songSelected.artist
                                ++ ": "
                                ++ songSelected.title

        basename : UriText
        basename =
            "append.php"

        index : Maybe SongRememberedIndex
        index =
            model.songRememberedCommentingIndex

        songSelected : Maybe SongRemembered
        songSelected =
            case index of
                Nothing ->
                    Nothing

                Just index ->
                    List.head (List.drop index model.songsRemembered)

        timeStamp : UriText
        timeStamp =
            case index of
                Nothing ->
                    ""

                Just _ ->
                    case songSelected of
                        Nothing ->
                            ""

                        Just song ->
                            song.timeStamp
    in
    relative
        [ basename ]
        [ ( "timestamp", timeStamp )
        , ( "song", artistTimeTitle )
        , ( "comment", model.likeOrCommentText )
        ]


relative : QueryBeforeList -> QueryPairs -> UriText
relative queryBeforeList queryPairs =
    --See:
    --https://github.com/elm-lang/http/issues/10
    --https://github.com/elm-lang/url
    --https://github.com/evancz/elm-http
    --http://package.elm-lang.org/packages/elm-lang/http/latest
    --TODO: When elm-lang/url is updated to contain 'relative', replace this code:
    let
        escapeAll : UriText -> UriText
        escapeAll string =
            --See:
            --http://package.elm-lang.org/packages/elm-lang/http/latest/Http
            --TODO: Possibly, use Http.encodeUri instead:
            escapeHashes (escapeEqualsSigns (escapeAmpersands string))

        escapeAmpersands : UriText -> UriText
        escapeAmpersands string =
            String.join
                "%26"
                (String.split "&" string)

        escapeEqualsSigns : UriText -> UriText
        escapeEqualsSigns string =
            String.join
                "%3D"
                (String.split "=" string)

        escapeHashes : UriText -> UriText
        escapeHashes string =
            String.join
                "%23"
                (String.split "#" string)

        query : UriText
        query =
            String.join
                "&"
                (List.map queryPairJoin queryPairs)

        queryBefore : UriText
        queryBefore =
            String.join
                "/"
                queryBeforeList

        queryPairJoin : QueryPair -> UriText
        queryPairJoin ( name, value ) =
            String.join
                "="
                [ name
                , escapeAll value
                ]
    in
    queryBefore ++ "?" ++ query


songGroup2String : SongGroup -> String
songGroup2String group =
    case group of
        Played ->
            "played"

        Remembered ->
            "remembered"


songView : Model -> SongGroup -> SongIndex -> SongRemembered -> Html Msg
songView model group index song =
    let
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
            if model.pageIsExpanded then
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
            , buySongAnchor song
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
            --See:
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
        alertArea : Html Msg
        alertArea =
            section
                [ id "alert" ]
                [ p []
                    [ text model.alertMessage ]
                ]

        songsLatestFew : List (Html Msg)
        songsLatestFew =
            List.indexedMap (songView model Played) songsLatestFew2Remembered

        songsLatestFew2Remembered : SongsRemembered
        songsLatestFew2Remembered =
            List.map songLatestFew2Remembered model.songsLatestFew

        songsRemembered : List (Html Msg)
        songsRemembered =
            List.indexedMap (songView model Remembered) model.songsRemembered
    in
    main_
        []
        [ alertArea
        , commentAreaPossibly model
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
