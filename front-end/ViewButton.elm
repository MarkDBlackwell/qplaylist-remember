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
        , buttonForgetRemember
        , buttonLike
        , buttonMy
        , buttonPlayed
        , buttonRemembered
        )

import Dom
    exposing
        ( Id
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
import MessageType
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
import ModelType
    exposing
        ( ShowCommentButtons
        )
import Song
    exposing
        ( SongGroup
            ( Played
            , Remembered
            )
        , SongsLatestOrRememberedIndex
        , SongsRememberedIndex
        )
import Utilities
    exposing
        ( maybeMapWithDefault
        )
import ViewType
    exposing
        ( Display
        , HoverText
        )
import ViewUtilities
    exposing
        ( htmlNodeNull
        , songGroup2String
        )


-- VIEW


buttonComment : SongGroup -> SongsRememberedIndex -> ShowCommentButtons -> Html Msg
buttonComment group songsRememberedIndex showCommentButtons =
    let
        buttonAction : Msg
        buttonAction =
            CommentAreaOpenHand songsRememberedIndex

        buttonId : Maybe Id
        buttonId =
            Just
                ("buttonComment"
                    ++ toString songsRememberedIndex
                )

        hoverText : HoverText
        hoverText =
            "Share a comment (with the DJ) about this song"
    in
    if Remembered == group then
        buttonMyComment buttonId hoverText buttonAction showCommentButtons
    else
        htmlNodeNull


buttonForgetRemember : SongGroup -> SongsLatestOrRememberedIndex -> Html Msg
buttonForgetRemember group songsLatestOrRememberedIndex =
    let
        buttonAction : Msg
        buttonAction =
            case group of
                Played ->
                    SongRememberHand songsLatestOrRememberedIndex

                Remembered ->
                    SongForgetHand songsLatestOrRememberedIndex

        buttonId : Maybe Id
        buttonId =
            Just
                ("button"
                    ++ songGroup2String group
                    ++ toString songsLatestOrRememberedIndex
                )

        hoverText : HoverText
        hoverText =
            case group of
                Played ->
                    "Add this song (to remembered songs)"

                Remembered ->
                    "Drop this song (from remembered songs)"
    in
    buttonMy buttonId hoverText buttonAction


buttonLike : SongGroup -> SongsRememberedIndex -> Html Msg
buttonLike group songsRememberedIndex =
    let
        buttonAction : Msg
        buttonAction =
            LikeButtonProcessHand songsRememberedIndex

        buttonId : Maybe Id
        buttonId =
            Just
                ("buttonLike"
                    ++ toString songsRememberedIndex
                )

        hoverText : HoverText
        hoverText =
            "Share a 'Like' (with the DJ) about this song"
    in
    case group of
        Played ->
            htmlNodeNull

        Remembered ->
            buttonMy buttonId hoverText buttonAction


buttonMy : Maybe Id -> HoverText -> Msg -> Html Msg
buttonMy buttonId hoverText action =
    buttonMyComment buttonId hoverText action False


buttonMyComment : Maybe Id -> HoverText -> Msg -> ShowCommentButtons -> Html Msg
buttonMyComment buttonId hoverText action showCommentButtons =
    let
        buttonIdView : List (Attribute msg)
        buttonIdView =
            maybeMapWithDefault [] (\x -> [ id x ]) buttonId

        display : Display
        display =
            let
                nonePossibly buttonId =
                    if
                        String.startsWith "buttonComment" buttonId
                            && not showCommentButtons
                    then
                        "none"
                    else
                        "inline-block"
            in
            maybeMapWithDefault "inline-block" nonePossibly buttonId
    in
    button
        ([ style [ ( "display", display ) ]
         , onClick action
         , title hoverText
         , type_ "button"
         ]
            ++ buttonIdView
        )
        []


buttonPlayed : Html Msg
buttonPlayed =
    let
        buttonId : Maybe Id
        buttonId =
            Just "refresh"

        hoverText : HoverText
        hoverText =
            "Refresh the latest few songs"
    in
    buttonMy buttonId hoverText SongsLatestRefreshHand


buttonRemembered : Html Msg
buttonRemembered =
    let
        buttonId : Maybe Id
        buttonId =
            Just "morph"

        hoverText : HoverText
        hoverText =
            "Morph this page's shape"
    in
    buttonMy buttonId hoverText PageMorphHand
