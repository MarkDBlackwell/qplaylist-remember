{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module View exposing (view)

import ElmCycle
    exposing
        ( Msg(..)
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
        , style
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
        ( SongGroup(..)
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
                                |> String.fromInt
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
                                    String.concat
                                        [ youShared
                                        , likes
                                        , orComments
                                        , aboutSong
                                        ]
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
                                styleCalc songGroup lengthRemembered songsRecentOrRememberedIndex

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
                --Per client request, the 'refresh' button is at the top.
                [ buttonRecent ]
             ]
                ++ songGroupView Remembered model.songsRemembered
            )
        , hr attributesEmpty innerHtmlEmpty
        , section
            (songGroupAttributes Recent)
            ([ p attributesEmpty
                [ buttonRemembered ]
             ]
                ++ (songs2SongsRemembered model.songsRecent
                        |> songGroupView Recent
                   )
            )
        ]
