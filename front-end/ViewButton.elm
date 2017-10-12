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
buttonComment group songsRememberedIndex showCommentButtons =
    let
        buttonAction : Msg
        buttonAction =
            CommentAreaOpenHand songsRememberedIndex

        buttonIdMaybe : IdMaybe
        buttonIdMaybe =
            toString songsRememberedIndex
                |> (++) "buttonComment"
                |> Just

        hoverText : HoverText
        hoverText =
            "Share a comment (with the DJ) about this song"
    in
    if Remembered == group then
        buttonMyComment buttonIdMaybe hoverText buttonAction showCommentButtons
    else
        htmlNodeNull


buttonForgetRemember : SongGroup -> SongsLatestOrRememberedIndex -> Html Msg
buttonForgetRemember group songsLatestOrRememberedIndex =
    let
        buttonAction : Msg
        buttonAction =
            case group of
                Latest ->
                    SongRememberHand songsLatestOrRememberedIndex

                Remembered ->
                    SongForgetHand songsLatestOrRememberedIndex

        buttonIdMaybe : IdMaybe
        buttonIdMaybe =
            Just
                (String.concat
                    [ "button"
                    , songGroup2String group
                    , toString songsLatestOrRememberedIndex
                    ]
                )

        hoverText : HoverText
        hoverText =
            case group of
                Latest ->
                    "Add this song (to remembered songs)"

                Remembered ->
                    "Drop this song (from remembered songs)"
    in
    buttonMy buttonIdMaybe hoverText buttonAction


buttonLike : SongGroup -> SongsRememberedIndex -> Html Msg
buttonLike group songsRememberedIndex =
    let
        buttonAction : Msg
        buttonAction =
            LikeButtonProcessHand songsRememberedIndex

        buttonIdMaybe : IdMaybe
        buttonIdMaybe =
            Just
                (toString songsRememberedIndex
                    |> (++) "buttonLike"
                )

        hoverText : HoverText
        hoverText =
            "Share a 'Like' (with the DJ) about this song"
    in
    case group of
        Latest ->
            htmlNodeNull

        Remembered ->
            buttonMy buttonIdMaybe hoverText buttonAction


buttonMy : IdMaybe -> HoverText -> Msg -> Html Msg
buttonMy buttonIdMaybe hoverText action =
    buttonMyComment buttonIdMaybe hoverText action False


buttonMyComment : IdMaybe -> HoverText -> Msg -> ShowCommentButtons -> Html Msg
buttonMyComment buttonIdMaybe hoverText action showCommentButtons =
    let
        buttonIdView : List (Attribute msg)
        buttonIdView =
            maybeMapWithDefault
                []
                (\x -> [ id x ])
                buttonIdMaybe

        display : Display
        display =
            let
                nonePossibly : Id -> HoverText
                nonePossibly buttonId =
                    if
                        String.startsWith "buttonComment" buttonId
                            && not showCommentButtons
                    then
                        "none"
                    else
                        "inline-block"
            in
            maybeMapWithDefault "inline-block" nonePossibly buttonIdMaybe
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
        buttonIdMaybe : IdMaybe
        buttonIdMaybe =
            Just "refresh"

        hoverText : HoverText
        hoverText =
            "Refresh the latest few songs"
    in
    buttonMy buttonIdMaybe hoverText SongsLatestRefreshHand


buttonRemembered : Html Msg
buttonRemembered =
    let
        buttonIdMaybe : IdMaybe
        buttonIdMaybe =
            Just "morph"

        hoverText : HoverText
        hoverText =
            "Morph this page's shape"
    in
    buttonMy buttonIdMaybe hoverText PageMorphHand
