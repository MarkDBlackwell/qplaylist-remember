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
import Message
    exposing
        ( Msg
            ( CommentAreaInputTextChangeCaptureHand
            , CommentCancelHand
            , CommentSendHand
            )
        )
import Model
    exposing
        ( Model
        )
import Song
    exposing
        ( SongBasic
        , SongGroup
            ( Played
            , Remembered
            )
        , SongGroupLength
        , SongRemembered
        , SongsLatestOrRememberedIndex
        , SongsRemembered
        , songsBasic2SongsRemembered
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
import ViewUtilities
    exposing
        ( htmlNodeNull
        , showCommentButtons
        , songGroup2String
        )


-- VIEW


commentArea : Model -> SongBasic -> Html Msg
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
            " – "
                ++ toString commentTextLength

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
                (song.artist
                    ++ ": "
                    ++ song.title
                    ++ " ("
                    ++ song.time
                    ++ " on "
                    ++ yearMonthDay
                    ++ ")"
                 --++ statistics
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
    case model.songCommenting of
        Nothing ->
            htmlNodeNull

        Just songCommenting ->
            commentArea model songCommenting


groupAttributes : SongGroup -> List (Attribute msg)
groupAttributes group =
    [ class "songs-group"
    , id
        ("songs-"
            ++ songGroup2String group
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
                            if showCommentButtons then
                                " (or a comment)"
                            else
                                ""
                    in
                    "You've shared a 'Like'"
                        ++ likedOrCommentedIndicatorHoverTextCommentButton
                        ++ " about this song (with the DJ)"
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
    in
    div
        songAttributes
        [ p []
            [ buttonForgetRemember group songsLatestOrRememberedIndex
            , span []
                [ text song.time ]
            , buttonComment group songsLatestOrRememberedIndex
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
                    [ text model.alertMessageText ]
                ]

        songsLatestView : List (Html Msg)
        songsLatestView =
            let
                songsLatestExpanded : SongsRemembered
                songsLatestExpanded =
                    songsBasic2SongsRemembered model.songsLatest
            in
            List.indexedMap (songView model Played) songsLatestExpanded

        songsRememberedView : List (Html Msg)
        songsRememberedView =
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
