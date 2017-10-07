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
        ( view
        )

import Html
    exposing
        ( Attribute
        , Html
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
        , id
        , placeholder
        , required
        , title
        , type_
        )
import Html.Events
    exposing
        ( onInput
        )
import MessageType
    exposing
        ( Msg
            ( CommentAreaInputTextChangeCaptureHand
            , CommentCancelHand
            , CommentSendHand
            )
        )
import ModelType
    exposing
        ( Model
        )
import Song
    exposing
        ( songs2SongsRemembered
        )
import SongType
    exposing
        ( SongGroup
            ( Played
            , Remembered
            )
        , SongGroupLength
        , SongLatest
        , SongRemembered
        , SongsLatestOrRememberedIndex
        , SongsRemembered
        , Time
        )
import Utilities
    exposing
        ( htmlNodeNull
        , maybeMapWithDefault
        , selectOneMaybe
        , songGroup2String
        )
import ViewButton
    exposing
        ( buttonComment
        , buttonForgetRemember
        , buttonLike
        , buttonMy
        , buttonPlayed
        , buttonRemembered
        )
import ViewBuySong
    exposing
        ( buySongAnchor
        )
import ViewStyleCalc
    exposing
        ( styleCalc
        )
import ViewType
    exposing
        ( HoverText
        )


-- VIEW


commentArea : Model -> SongLatest -> Html Msg
commentArea model song =
    let
        hoverText : HoverText
        hoverText =
            "Type your (additional) comment here!"

        statistics : String
        statistics =
            let
                commentTextLength : Int
                commentTextLength =
                    String.length model.commentText
            in
            toString commentTextLength
                |> (++) " – "

        yearMonthDay : String
        yearMonthDay =
            let
                timestampFieldsSelected : List String
                timestampFieldsSelected =
                    let
                        howManyToTake : Int
                        howManyToTake =
                            3

                        timestampList : List String
                        timestampList =
                            String.split " " song.timestamp
                    in
                    List.take howManyToTake timestampList
            in
            String.join "-" timestampFieldsSelected
    in
    section
        [ id "comment" ]
        [ p []
            [ text
                (String.concat
                    [ song.artist
                    , ": "
                    , song.title
                    , " ("
                    , song.time
                    , " on "
                    , yearMonthDay
                    , ")"

                    --, statistics
                    ]
                )
            ]
        , input
            [ autocomplete False
            , id "input"
            , onInput CommentAreaInputTextChangeCaptureHand
            , placeholder hoverText
            , required True
            , title hoverText
            , type_ "text"
            ]
            []
        , buttonMy Nothing "Submit your comment" CommentSendHand
        , buttonMy Nothing "Cancel this comment" CommentCancelHand
        ]


commentAreaPossibly : Model -> Html Msg
commentAreaPossibly model =
    maybeMapWithDefault
        htmlNodeNull
        (\x -> commentArea model x)
        model.songCommentingMaybe


groupAttributes : SongGroup -> List (Attribute msg)
groupAttributes group =
    [ class "songs-group"
    , id
        (songGroup2String group
            |> (++) "songs-"
        )
    ]


songView : Model -> SongGroup -> SongsLatestOrRememberedIndex -> SongRemembered -> Html Msg
songView model group songsLatestOrRememberedIndex song =
    let
        likedOrCommentedIndicator : Html Msg
        likedOrCommentedIndicator =
            let
                likedOrCommentedIndicatorHoverText : HoverText
                likedOrCommentedIndicatorHoverText =
                    let
                        likedOrCommentedIndicatorHoverTextCommentButton : HoverText
                        likedOrCommentedIndicatorHoverTextCommentButton =
                            if model.showCommentButtons then
                                " (or a comment)"
                            else
                                ""
                    in
                    String.concat
                        [ "You've shared a 'Like'"
                        , likedOrCommentedIndicatorHoverTextCommentButton
                        , " about this song (with the DJ)"
                        ]
            in
            if song.likedOrCommented then
                em [ title likedOrCommentedIndicatorHoverText ]
                    []
            else
                htmlNodeNull

        songAttributes : List (Attribute msg)
        songAttributes =
            let
                lengthRemembered : SongGroupLength
                lengthRemembered =
                    List.length model.songsRemembered
            in
            if model.pageIsExpanded then
                []
            else
                [ styleCalc group lengthRemembered songsLatestOrRememberedIndex ]

        songTime : Time
        songTime =
            let
                prefix : String
                prefix =
                    String.concat
                        [ select 0
                        , " "
                        , select 1
                        , "-"
                        , select 2
                        , " "
                        ]

                select : Int -> String
                select index =
                    selectOneMaybe stampList index
                        |> Maybe.withDefault ""

                stampList : List String
                stampList =
                    String.split " " song.timestamp
            in
            case group of
                Played ->
                    song.time

                Remembered ->
                    prefix ++ song.time
    in
    div
        songAttributes
        [ p []
            [ buttonForgetRemember group songsLatestOrRememberedIndex
            , span []
                [ text songTime ]
            , buttonComment group songsLatestOrRememberedIndex model.showCommentButtons
            , buttonLike group songsLatestOrRememberedIndex
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
                    [ text (Maybe.withDefault "" model.alertMessageText) ]
                ]

        songsLatestView : List (Html Msg)
        songsLatestView =
            let
                songsLatestExpanded : SongsRemembered
                songsLatestExpanded =
                    songs2SongsRemembered model.songsLatest
            in
            List.indexedMap
                (songView model Played)
                songsLatestExpanded

        songsRememberedView : List (Html Msg)
        songsRememberedView =
            List.indexedMap
                (songView model Remembered)
                model.songsRemembered
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
                ++ songsRememberedView
            )
        , hr [] []
        , section
            (groupAttributes Played)
            ([ p []
                [ buttonPlayed ]
             ]
                ++ songsLatestView
            )
        ]
