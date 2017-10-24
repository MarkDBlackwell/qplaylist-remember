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
        ( SongGroup
            ( Recent
            , Remembered
            )
        , SongGroupLength
        , SongRecentOrRemembered
        , SongRemembered
        , SongsRecentOrRemembered
        , SongsRecentOrRememberedIndex
        , Time
        )
import Utilities
    exposing
        ( attributesEmpty
        , htmlNodeNull
        , innerHtmlEmpty
        , maybeMapWithDefault
        , selectOneFromIndexMaybe
        )
import ViewButton
    exposing
        ( buttonComment
        , buttonLike
        , buttonRecent
        , buttonRememberForget
        , buttonRemembered
        , buttonView
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
                [ p attributesEmpty
                    [ text (Maybe.withDefault "" model.alertMessageText) ]
                ]

        commentAreaPossibly : Html Msg
        commentAreaPossibly =
            let
                commentArea : SongRemembered -> Html Msg
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
                        [ p attributesEmpty
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
                            innerHtmlEmpty
                        , buttonView Nothing "Submit your comment" CommentSendHand
                        , buttonView Nothing "Cancel this comment" CommentCancelHand
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

        songGroupView : SongGroup -> SongsRecentOrRemembered -> List (Html Msg)
        songGroupView songGroup songsRecentOrRemembered =
            let
                songView : SongsRecentOrRememberedIndex -> SongRecentOrRemembered -> Html Msg
                songView songsRecentOrRememberedIndex songRecentOrRemembered =
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
                            if songRecentOrRemembered.likedOrCommented then
                                em [ title indicatorHoverText ]
                                    innerHtmlEmpty
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
                                attributesEmpty
                            else
                                [ styleCalc songGroup lengthRemembered songsRecentOrRememberedIndex ]

                        songTime : Time
                        songTime =
                            let
                                clock : Time
                                clock =
                                    songRecentOrRemembered.time

                                prefix : String
                                prefix =
                                    let
                                        select : Int -> String
                                        select index =
                                            let
                                                stampList : List String
                                                stampList =
                                                    String.split " " songRecentOrRemembered.timestamp
                                            in
                                            selectOneFromIndexMaybe stampList index
                                                |> Maybe.withDefault ""
                                    in
                                    String.concat
                                        [ select 0
                                        , " "
                                        , select 1
                                        , "-"
                                        , select 2
                                        , " "
                                        ]
                            in
                            case songGroup of
                                Recent ->
                                    clock

                                Remembered ->
                                    prefix ++ clock
                    in
                    div
                        songAttributes
                        [ p attributesEmpty
                            [ buttonRememberForget songGroup songsRecentOrRememberedIndex
                            , span attributesEmpty
                                [ text songTime ]
                            , buttonComment songGroup songsRecentOrRememberedIndex model.showCommentButtons
                            , buttonLike songGroup songsRecentOrRememberedIndex
                            , likedOrCommentedIndicator
                            , buySongAnchor songRecentOrRemembered
                            ]
                        , p attributesEmpty
                            [ text songRecentOrRemembered.title ]
                        , p attributesEmpty
                            [ text songRecentOrRemembered.artist ]
                        ]
            in
            List.indexedMap songView songsRecentOrRemembered
    in
    main_
        attributesEmpty
        [ alertArea
        , commentAreaPossibly
        , section
            (songGroupAttributes Remembered)
            ([ p attributesEmpty
                [ buttonRemembered ]
             ]
                ++ songGroupView Remembered model.songsRemembered
            )
        , hr attributesEmpty innerHtmlEmpty
        , section
            (songGroupAttributes Recent)
            ([ p attributesEmpty
                [ buttonRecent ]
             ]
                ++ (songs2SongsRemembered model.songsRecent
                        |> songGroupView Recent
                   )
            )
        ]
