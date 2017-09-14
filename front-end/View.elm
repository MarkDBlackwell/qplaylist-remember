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
import Html.Events exposing (onInput)
import MessageDetails
    exposing
        ( Msg
            ( CommentAreaInputTextChangeCaptureHand
            , CommentCancelHand
            , CommentSendHand
            )
        )
import ModelDetails
    exposing
        ( Model
        , SongBasic
        , SongCommentingIndex
        , SongRemembered
        , SongsRemembered
        , songBasic2SongRemembered
        )
import ModelDetailsUpdate
    exposing
        ( SongRememberedIndex
        )
import ModelDetailsView
    exposing
        ( HoverText
        , SongGroup
            ( Played
            , Remembered
            )
        , SongGroupLength
        , SongIndex
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
import ViewBuySong exposing (buySongAnchor)
import ViewStyleCalc exposing (styleCalc)
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
            " – "
                ++ song.timeStamp
                ++ ": "
                ++ toString (String.length model.commentText)
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
    let
        songMaybe : SongRememberedIndex -> Maybe SongRemembered
        songMaybe index =
            List.head (List.drop index model.songsRemembered)
    in
    if not model.processingComment then
        htmlNodeNull
    else
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
                    [ text model.alertMessageText ]
                ]

        songsLatestFew : List (Html Msg)
        songsLatestFew =
            List.indexedMap (songView model Played) songsLatestFew2Remembered

        songsLatestFew2Remembered : SongsRemembered
        songsLatestFew2Remembered =
            List.map songBasic2SongRemembered model.songsLatestFew

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
