{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module ViewButton exposing
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
    exposing
        ( Html
        , button
        )
import Html.Attributes
    exposing
        ( style
        , title
        , type_
        )
import Html.Events
    exposing
        ( onClick
        )
import ModelType
    exposing
        ( ShowCommentButtons
        )
import SongHelper
    exposing
        ( songGroup2String
        )
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


buttonComment : SongGroup -> SongsRememberedIndex -> ShowCommentButtons -> Html Msg
buttonComment songGroup songsRememberedIndex showCommentButtons =
    let
        buttonActionMsg : Msg
        buttonActionMsg =
            songsRememberedIndex
                |> CommentAreaOpenHand

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


buttonCommentView : IdMaybe -> HoverText -> Msg -> ShowCommentButtons -> Html Msg
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
    button
        ([ style "display" displayValue
         , onClick action
         , title hoverText
         , type_ "button"
         ]
            ++ attributeIdFromMaybe buttonAttributeIdMaybe
        )
        innerHtmlEmpty


buttonLike : SongGroup -> SongsRememberedIndex -> Html Msg
buttonLike songGroup songsRememberedIndex =
    let
        buttonActionMsg : Msg
        buttonActionMsg =
            songsRememberedIndex
                |> LikeButtonProcessHand

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


buttonRecent : Html Msg
buttonRecent =
    let
        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            Just "refresh"

        hoverText : HoverText
        hoverText =
            "Refresh the latest few songs"
    in
    SongsRecentRefreshHand
        |> buttonView buttonAttributeIdMaybe hoverText


buttonRememberForget : SongGroup -> SongsRecentOrRememberedIndex -> Html Msg
buttonRememberForget songGroup songsRecentOrRememberedIndex =
    let
        buttonActionMsg : Msg
        buttonActionMsg =
            case songGroup of
                Recent ->
                    songsRecentOrRememberedIndex
                        |> SongRememberHand

                Remembered ->
                    songsRecentOrRememberedIndex
                        |> SongForgetHand

        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            [ "button"
            , songGroup2String songGroup
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


buttonRemembered : Html Msg
buttonRemembered =
    let
        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            Just "morph"

        hoverText : HoverText
        hoverText =
            "Morph this page's shape"
    in
    PageMorphHand
        |> buttonView buttonAttributeIdMaybe hoverText


buttonView : IdMaybe -> HoverText -> Msg -> Html Msg
buttonView buttonAttributeIdMaybe hoverText action =
    False
        |> buttonCommentView buttonAttributeIdMaybe hoverText action
