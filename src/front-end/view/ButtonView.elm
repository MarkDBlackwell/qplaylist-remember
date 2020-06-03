{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ButtonView exposing
    ( buttonComment
    , buttonLike
    , buttonRecent
    , buttonRememberForget
    , buttonRemembered
    , buttonView
    )

import ElmCycle
    exposing
        ( Msg(..)
        )
import Html
import Html.Attributes
import Html.Events
import ModelType
    exposing
        ( ShowCommentButtons
        )
import SongHelper
import SongType
    exposing
        ( SongGroup(..)
        , SongsRecentOrRememberedIndex
        , SongsRememberedIndex
        )
import Utilities
    exposing
        ( attributeIdFromMaybe
        , htmlNodeNull
        , innerHtmlEmpty
        , maybeMapWithDefault
        )
import ViewType
    exposing
        ( Display
        , HoverText
        , Id
        , IdMaybe
        )



-- VIEW


buttonComment : SongGroup -> SongsRememberedIndex -> ShowCommentButtons -> Html.Html ElmCycle.Msg
buttonComment songGroup songsRememberedIndex showCommentButtons =
    let
        buttonActionMsg : ElmCycle.Msg
        buttonActionMsg =
            songsRememberedIndex
                |> MsgCommentAreaOpenHand

        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            songsRememberedIndex
                |> String.fromInt
                |> (++) "buttonComment"
                |> Just

        hoverText : HoverText
        hoverText =
            "Share a comment (with the DJ) about this song"
    in
    if Recent == songGroup then
        htmlNodeNull

    else
        showCommentButtons
            |> buttonCommentView buttonAttributeIdMaybe hoverText buttonActionMsg


buttonCommentView : IdMaybe -> HoverText -> ElmCycle.Msg -> ShowCommentButtons -> Html.Html ElmCycle.Msg
buttonCommentView buttonAttributeIdMaybe hoverText action showCommentButtons =
    let
        displayValue : Display
        displayValue =
            let
                default : Display
                default =
                    "inline-block"

                nonePossibly : Id -> HoverText
                nonePossibly buttonId =
                    if
                        String.startsWith "buttonComment" buttonId
                            && not showCommentButtons
                    then
                        "none"

                    else
                        default
            in
            maybeMapWithDefault default nonePossibly buttonAttributeIdMaybe
    in
    Html.button
        ([ Html.Attributes.style "display" displayValue
         , Html.Events.onClick action
         , Html.Attributes.title hoverText
         , Html.Attributes.type_ "button"
         ]
            ++ attributeIdFromMaybe buttonAttributeIdMaybe
        )
        innerHtmlEmpty


buttonLike : SongGroup -> SongsRememberedIndex -> Html.Html ElmCycle.Msg
buttonLike songGroup songsRememberedIndex =
    let
        buttonActionMsg : ElmCycle.Msg
        buttonActionMsg =
            songsRememberedIndex
                |> MsgLikeSendHand

        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            Just
                (songsRememberedIndex
                    |> String.fromInt
                    |> (++) "buttonLike"
                )

        hoverText : HoverText
        hoverText =
            "Share a 'Like' (with the DJ) about this song"
    in
    case songGroup of
        Recent ->
            htmlNodeNull

        Remembered ->
            buttonActionMsg
                |> buttonView buttonAttributeIdMaybe hoverText


buttonRecent : Html.Html ElmCycle.Msg
buttonRecent =
    let
        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            Just "refresh"

        hoverText : HoverText
        hoverText =
            "Refresh the latest few songs"
    in
    MsgSongsRecentRefreshHand
        |> buttonView buttonAttributeIdMaybe hoverText


buttonRememberForget : SongGroup -> SongsRecentOrRememberedIndex -> Html.Html ElmCycle.Msg
buttonRememberForget songGroup songsRecentOrRememberedIndex =
    let
        buttonActionMsg : ElmCycle.Msg
        buttonActionMsg =
            case songGroup of
                Recent ->
                    songsRecentOrRememberedIndex
                        |> MsgSongRememberHand

                Remembered ->
                    songsRecentOrRememberedIndex
                        |> MsgSongForgetHand

        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            [ "button"
            , SongHelper.songGroup2String songGroup
            , String.fromInt songsRecentOrRememberedIndex
            ]
                |> String.concat
                |> Just

        hoverText : HoverText
        hoverText =
            case songGroup of
                Recent ->
                    "Add this song (to remembered songs)"

                Remembered ->
                    "Drop this song (from remembered songs)"
    in
    buttonActionMsg
        |> buttonView buttonAttributeIdMaybe hoverText


buttonRemembered : Html.Html ElmCycle.Msg
buttonRemembered =
    let
        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            Just "morph"

        hoverText : HoverText
        hoverText =
            "Morph this page's shape"
    in
    MsgPageMorphHand
        |> buttonView buttonAttributeIdMaybe hoverText


buttonView : IdMaybe -> HoverText -> ElmCycle.Msg -> Html.Html ElmCycle.Msg
buttonView buttonAttributeIdMaybe hoverText action =
    False
        |> buttonCommentView buttonAttributeIdMaybe hoverText action
