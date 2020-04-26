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

import Dom
    exposing
        ( Id
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
        , IdMaybe
        )



-- VIEW


buttonComment : SongGroup -> SongsRememberedIndex -> ShowCommentButtons -> Html Msg
buttonComment songGroup songsRememberedIndex showCommentButtons =
    let
        buttonActionMsg : Msg
        buttonActionMsg =
            CommentAreaOpenHand songsRememberedIndex

        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            toString songsRememberedIndex
                |> (++) "buttonComment"
                |> Just

        hoverText : HoverText
        hoverText =
            "Share a comment (with the DJ) about this song"
    in
    if Recent == songGroup then
        htmlNodeNull

    else
        buttonCommentView buttonAttributeIdMaybe hoverText buttonActionMsg showCommentButtons


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
            LikeButtonProcessHand songsRememberedIndex

        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            Just
                (toString songsRememberedIndex
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
            buttonView buttonAttributeIdMaybe hoverText buttonActionMsg


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
    buttonView buttonAttributeIdMaybe hoverText SongsRecentRefreshHand


buttonRememberForget : SongGroup -> SongsRecentOrRememberedIndex -> Html Msg
buttonRememberForget songGroup songsRecentOrRememberedIndex =
    let
        buttonActionMsg : Msg
        buttonActionMsg =
            case songGroup of
                Recent ->
                    SongRememberHand songsRecentOrRememberedIndex

                Remembered ->
                    SongForgetHand songsRecentOrRememberedIndex

        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            Just
                (String.concat
                    [ "button"
                    , songGroup2String songGroup
                    , toString songsRecentOrRememberedIndex
                    ]
                )

        hoverText : HoverText
        hoverText =
            case songGroup of
                Recent ->
                    "Add this song (to remembered songs)"

                Remembered ->
                    "Drop this song (from remembered songs)"
    in
    buttonView buttonAttributeIdMaybe hoverText buttonActionMsg


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
    buttonView buttonAttributeIdMaybe hoverText PageMorphHand


buttonView : IdMaybe -> HoverText -> Msg -> Html Msg
buttonView buttonAttributeIdMaybe hoverText action =
    buttonCommentView buttonAttributeIdMaybe hoverText action False
