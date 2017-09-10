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


module View
    exposing
        ( likeOrCommentRequestUriText
        , view
        )

import Html
    exposing
        ( Attribute
        , Html
        , a
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
        , target
        , title
        , type_
        )
import Html.Events
    exposing
        ( onClick
        , onInput
        )
import MessageDetails exposing (..)
import ModelDetails
    exposing
        ( Model
        , SongRemembered
        , SongsRemembered
        , songLatestFew2Remembered
        )
import ModelDetailsUpdate exposing (..)
import ModelDetailsView exposing (..)
import ViewButton exposing (..)
import ViewStyleCalc exposing (styleCalc)
import ViewUtilities
    exposing
        ( htmlNodeNull
        , relative
        , showCommentButtons
        , songGroup2String
        )


-- VIEW


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
