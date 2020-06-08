{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module View exposing (view)

import ButtonView
    exposing
        ( buttonComment
        , buttonLike
        , buttonRecent
        , buttonRememberForget
        , buttonRemembered
        , buttonView
        )
import BuySongView
import ElmCycle
    exposing
        ( Msg(..)
        )
import Html
    exposing
        ( div
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
        , style
        , title
        , type_
        )
import Html.Events
import ModelType
    exposing
        ( Model
        )
import SongHelper
import SongType
    exposing
        ( SongGroup(..)
        , SongGroupLength
        , SongRecentOrRemembered
        , SongRemembered
        , SongsRecentOrRemembered
        , SongsRecentOrRememberedIndex
        , Time
        )
import StyleCalcView
import Utilities
    exposing
        ( attributesEmpty
        , htmlNodeNull
        , innerHtmlEmpty
        , maybeMapWithDefault
        , selectOneFromIndexMaybe
        )
import ViewType
    exposing
        ( HoverText
        )



-- VIEW


view : Model -> Html.Html ElmCycle.Msg
view model =
    let
        alertArea : Html.Html ElmCycle.Msg
        alertArea =
            section
                [ id "alert" ]
                [ p
                    attributesEmpty
                    [ Maybe.withDefault "" model.alertMessageText
                        |> text
                    ]
                ]

        commentAreaPossibly : Html.Html ElmCycle.Msg
        commentAreaPossibly =
            let
                commentArea : SongRemembered -> Html.Html ElmCycle.Msg
                commentArea song =
                    let
                        artistTitleTime : String
                        artistTitleTime =
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
                                |> String.concat

                        inputHoverText : HoverText
                        inputHoverText =
                            "Type your (additional) comment here!"

                        statistics : String
                        statistics =
                            model.commentText
                                |> String.length
                                |> String.fromInt
                                |> String.append " – "

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
                                    song.timestamp
                                        |> String.split " "
                                        |> List.take
                                            howManyToTake
                            in
                            timestampFieldsSelected
                                |> String.join "-"
                    in
                    section
                        [ id "comment" ]
                        [ p
                            attributesEmpty
                            [ text artistTitleTime ]
                        , input
                            [ autocomplete False
                            , id "input"
                            , Html.Events.onInput
                                MsgCommentAreaInputTextChangeCaptureHand
                            , placeholder
                                inputHoverText
                            , required True
                            , title inputHoverText
                            , type_ "text"
                            ]
                            innerHtmlEmpty
                        , MsgCommentSendHand
                            |> buttonView Nothing "Submit your comment"
                        , MsgCommentCancelHand
                            |> buttonView Nothing "Cancel this comment"
                        ]
            in
            model.songCommentingMaybe
                |> maybeMapWithDefault
                    htmlNodeNull
                    (\x -> commentArea x)

        songGroupAttributes : SongGroup -> List (Html.Attribute ElmCycle.Msg)
        songGroupAttributes songGroup =
            [ class "songs-group"
            , id
                (songGroup
                    |> SongHelper.songGroup2String
                    |> String.append "songs-"
                )
            ]

        songGroupView : SongGroup -> SongsRecentOrRemembered -> List (Html.Html ElmCycle.Msg)
        songGroupView songGroup songsRecentOrRemembered =
            let
                songView : SongsRecentOrRememberedIndex -> SongRecentOrRemembered -> Html.Html ElmCycle.Msg
                songView songsRecentOrRememberedIndex songRecentOrRemembered =
                    let
                        likedOrCommentedIndicator : Html.Html ElmCycle.Msg
                        likedOrCommentedIndicator =
                            let
                                indicatorHoverText : HoverText
                                indicatorHoverText =
                                    let
                                        aboutSong : HoverText
                                        aboutSong =
                                            " about this song (with the DJ)"

                                        likes : HoverText
                                        likes =
                                            case songRecentOrRemembered.likeOrCommentCount of
                                                1 ->
                                                    " a 'Like'"

                                                _ ->
                                                    " 'Like's"

                                        orComments : HoverText
                                        orComments =
                                            let
                                                comments : HoverText
                                                comments =
                                                    case songRecentOrRemembered.likeOrCommentCount of
                                                        1 ->
                                                            " (or a comment)"

                                                        _ ->
                                                            " (or comments)"
                                            in
                                            if model.showCommentButtons then
                                                comments

                                            else
                                                ""

                                        youShared : HoverText
                                        youShared =
                                            "You've shared"
                                    in
                                    [ youShared
                                    , likes
                                    , orComments
                                    , aboutSong
                                    ]
                                        |> String.concat
                            in
                            case songRecentOrRemembered.likeOrCommentCount of
                                0 ->
                                    htmlNodeNull

                                1 ->
                                    em [ title indicatorHoverText ]
                                        innerHtmlEmpty

                                _ ->
                                    em
                                        [ title indicatorHoverText
                                        , style "background-color" "Salmon"
                                        , style "color" "Salmon"
                                        ]
                                        innerHtmlEmpty

                        songAttributes : List (Html.Attribute ElmCycle.Msg)
                        songAttributes =
                            let
                                lengthRemembered : SongGroupLength
                                lengthRemembered =
                                    model.songsRemembered
                                        |> List.length
                            in
                            if model.pageIsExpanded then
                                attributesEmpty

                            else
                                songsRecentOrRememberedIndex
                                    |> StyleCalcView.styleCalc
                                        songGroup
                                        lengthRemembered

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
                                                    songRecentOrRemembered.timestamp
                                                        |> String.split " "
                                            in
                                            index
                                                |> selectOneFromIndexMaybe stampList
                                                |> Maybe.withDefault ""
                                    in
                                    [ select 0
                                    , " "
                                    , select 1
                                    , "-"
                                    , select 2
                                    , " "
                                    ]
                                        |> String.concat
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
                            [ songsRecentOrRememberedIndex
                                |> buttonRememberForget songGroup
                            , span
                                attributesEmpty
                                [ text songTime ]
                            , model.showCommentButtons
                                |> buttonComment
                                    songGroup
                                    songsRecentOrRememberedIndex
                            , songsRecentOrRememberedIndex
                                |> buttonLike
                                    songGroup
                            , likedOrCommentedIndicator
                            , songRecentOrRemembered
                                |> BuySongView.buySongAnchor
                            ]
                        , p attributesEmpty
                            [ text songRecentOrRemembered.title ]
                        , p attributesEmpty
                            [ text songRecentOrRemembered.artist ]
                        ]
            in
            songsRecentOrRemembered
                |> List.indexedMap songView
    in
    main_
        attributesEmpty
        [ alertArea
        , commentAreaPossibly
        , section
            (songGroupAttributes Remembered)
            ([ p attributesEmpty
                --Per client request, the 'refresh' button is at the top.
                [ buttonRecent ]
             ]
                ++ (model.songsRemembered
                        |> songGroupView
                            Remembered
                   )
            )
        , hr
            attributesEmpty
            innerHtmlEmpty
        , section
            (songGroupAttributes Recent)
            ([ p
                attributesEmpty
                [ buttonRemembered ]
             ]
                ++ (model.songsRecent
                        |> SongHelper.songs2SongsRemembered
                        |> songGroupView
                            Recent
                   )
            )
        ]
