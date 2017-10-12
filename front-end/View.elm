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

import ElmCycle
    exposing
        ( Msg
            ( CommentAreaInputTextChangeCaptureHand
            , CommentCancelHand
            , CommentSendHand
            )
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
import ModelType
    exposing
        ( Model
        )
import SongHelper
    exposing
        ( songGroup2String
        , songs2SongsRemembered
        )
import SongType
    exposing
        ( SongCommenting
        , SongGroup
            ( Played
            , Remembered
            )
        , SongGroupLength
        , SongPlayedOrRemembered
        , SongsPlayedOrRemembered
        , SongsPlayedOrRememberedIndex
        , Time
        )
import Utilities
    exposing
        ( htmlNodeNull
        , maybeMapWithDefault
        , selectOneMaybe
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

        commentAreaPossibly : Html Msg
        commentAreaPossibly =
            let
                commentArea : SongCommenting -> Html Msg
                commentArea song =
                    let
                        inputHoverText : HoverText
                        inputHoverText =
                            "Type your (additional) comment here!"

                        statistics : String
                        statistics =
                            String.length model.commentText
                                |> toString
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
                                    in
                                    String.split " " song.timestamp
                                        |> List.take howManyToTake
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

                                    --For development, keep this statistics line.
                                    --, statistics
                                    ]
                                )
                            ]
                        , input
                            [ autocomplete False
                            , id "input"
                            , onInput CommentAreaInputTextChangeCaptureHand
                            , placeholder inputHoverText
                            , required True
                            , title inputHoverText
                            , type_ "text"
                            ]
                            []
                        , buttonMy Nothing "Submit your comment" CommentSendHand
                        , buttonMy Nothing "Cancel this comment" CommentCancelHand
                        ]
            in
            maybeMapWithDefault
                htmlNodeNull
                (\x -> commentArea x)
                model.songCommentingMaybe

        songGroupAttributes : SongGroup -> List (Attribute msg)
        songGroupAttributes songGroup =
            [ class "songs-group"
            , id
                (songGroup2String songGroup
                    |> (++) "songs-"
                )
            ]

        songGroupView : SongGroup -> SongsPlayedOrRemembered -> List (Html Msg)
        songGroupView songGroup songsPlayedOrRemembered =
            let
                songView : SongsPlayedOrRememberedIndex -> SongPlayedOrRemembered -> Html Msg
                songView songsPlayedOrRememberedIndex songPlayedOrRemembered =
                    let
                        likedOrCommentedIndicator : Html Msg
                        likedOrCommentedIndicator =
                            let
                                indicatorHoverText : HoverText
                                indicatorHoverText =
                                    let
                                        orAComment : HoverText
                                        orAComment =
                                            if model.showCommentButtons then
                                                " (or a comment)"
                                            else
                                                ""
                                    in
                                    String.concat
                                        [ "You've shared a 'Like'"
                                        , orAComment
                                        , " about this song (with the DJ)"
                                        ]
                            in
                            if songPlayedOrRemembered.likedOrCommented then
                                em [ title indicatorHoverText ]
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
                                [ styleCalc songGroup lengthRemembered songsPlayedOrRememberedIndex ]

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
                                    String.split " " songPlayedOrRemembered.timestamp
                            in
                            case songGroup of
                                Played ->
                                    songPlayedOrRemembered.time

                                Remembered ->
                                    prefix ++ songPlayedOrRemembered.time
                    in
                    div
                        songAttributes
                        [ p []
                            [ buttonForgetRemember songGroup songsPlayedOrRememberedIndex
                            , span []
                                [ text songTime ]
                            , buttonComment songGroup songsPlayedOrRememberedIndex model.showCommentButtons
                            , buttonLike songGroup songsPlayedOrRememberedIndex
                            , likedOrCommentedIndicator
                            , buySongAnchor songPlayedOrRemembered
                            ]
                        , p []
                            [ text songPlayedOrRemembered.title ]
                        , p []
                            [ text songPlayedOrRemembered.artist ]
                        ]
            in
            List.indexedMap songView songsPlayedOrRemembered
    in
    main_
        []
        [ alertArea
        , commentAreaPossibly
        , section
            (songGroupAttributes Remembered)
            ([ p []
                [ buttonRemembered ]
             ]
                ++ songGroupView Remembered model.songsRemembered
            )
        , hr [] []
        , section
            (songGroupAttributes Played)
            ([ p []
                [ buttonPlayed ]
             ]
                ++ (songs2SongsRemembered model.songsLatest
                        |> songGroupView Played
                   )
            )
        ]
