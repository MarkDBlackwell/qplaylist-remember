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


module ViewButton
    exposing
        ( buttonComment
        , buttonLatest
        , buttonLike
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
        ( Msg
            ( CommentAreaOpenHand
            , LikeButtonProcessHand
            , PageMorphHand
            , SongForgetHand
            , SongRememberHand
            , SongsLatestRefreshHand
            )
        )
import Html
    exposing
        ( Attribute
        , Html
        , button
        )
import Html.Attributes
    exposing
        ( id
        , style
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
        ( SongGroup
            ( Latest
            , Remembered
            )
        , SongsLatestOrRememberedIndex
        , SongsRememberedIndex
        )
import Utilities
    exposing
        ( htmlNodeNull
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
    if Latest == songGroup then
        htmlNodeNull
    else
        buttonCommentView buttonAttributeIdMaybe hoverText buttonActionMsg showCommentButtons


buttonCommentView : IdMaybe -> HoverText -> Msg -> ShowCommentButtons -> Html Msg
buttonCommentView buttonAttributeIdMaybe hoverText action showCommentButtons =
    let
        buttonAttributeId : List (Attribute msg)
        buttonAttributeId =
            maybeMapWithDefault
                []
                (\x -> [ id x ])
                buttonAttributeIdMaybe

        buttonInnerHtml : List (Html msg)
        buttonInnerHtml =
            []

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
        ([ style [ ( "display", displayValue ) ]
         , onClick action
         , title hoverText
         , type_ "button"
         ]
            ++ buttonAttributeId
        )
        buttonInnerHtml


buttonLatest : Html Msg
buttonLatest =
    let
        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            Just "refresh"

        hoverText : HoverText
        hoverText =
            "Refresh the latest few songs"
    in
    buttonView buttonAttributeIdMaybe hoverText SongsLatestRefreshHand


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
        Latest ->
            htmlNodeNull

        Remembered ->
            buttonView buttonAttributeIdMaybe hoverText buttonActionMsg


buttonRememberForget : SongGroup -> SongsLatestOrRememberedIndex -> Html Msg
buttonRememberForget songGroup songsLatestOrRememberedIndex =
    let
        buttonActionMsg : Msg
        buttonActionMsg =
            case songGroup of
                Latest ->
                    SongRememberHand songsLatestOrRememberedIndex

                Remembered ->
                    SongForgetHand songsLatestOrRememberedIndex

        buttonAttributeIdMaybe : IdMaybe
        buttonAttributeIdMaybe =
            Just
                (String.concat
                    [ "button"
                    , songGroup2String songGroup
                    , toString songsLatestOrRememberedIndex
                    ]
                )

        hoverText : HoverText
        hoverText =
            case songGroup of
                Latest ->
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
