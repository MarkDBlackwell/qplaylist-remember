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
import MessageDetails
    exposing
        ( Msg
            ( CommentAreaOpenHand
            , LikeButtonProcessHand
            , PageMorphHand
            , SongForgetHand
            , SongRememberHand
            , SongsLatestFewRefreshHand
            )
        )
import ModelDetailsUpdate
    exposing
        ( SongRememberedIndex
        )
import ModelDetailsView
    exposing
        ( Display
        , HoverText
        , SongGroup
            ( Played
            , Remembered
            )
        , SongLatestFewOrRememberedIndex
        )
import ViewUtilities
    exposing
        ( htmlNodeNull
        , showCommentButtons
        , songGroup2String
        )


-- VIEW


buttonComment : SongGroup -> SongRememberedIndex -> Html Msg
buttonComment group songRememberedIndex =
    let
        buttonAction : Msg
        buttonAction =
            CommentAreaOpenHand songRememberedIndex

        buttonId : Maybe Id
        buttonId =
            Just
                ("buttonComment"
                    ++ toString songRememberedIndex
                )

        hoverText : HoverText
        hoverText =
            "Share a comment (with the DJ) about this song"
    in
    if Remembered == group then
        buttonMy buttonId hoverText buttonAction
    else
        htmlNodeNull


buttonForgetRemember : SongGroup -> SongLatestFewOrRememberedIndex -> Html Msg
buttonForgetRemember group songLatestFewOrRememberedIndex =
    let
        buttonAction : Msg
        buttonAction =
            case group of
                Played ->
                    SongRememberHand songLatestFewOrRememberedIndex

                Remembered ->
                    SongForgetHand songLatestFewOrRememberedIndex

        buttonId : Maybe Id
        buttonId =
            Just
                ("button"
                    ++ songGroup2String group
                    ++ toString songLatestFewOrRememberedIndex
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


buttonLike : SongGroup -> SongRememberedIndex -> Html Msg
buttonLike group songRememberedIndex =
    let
        buttonAction : Msg
        buttonAction =
            LikeButtonProcessHand songRememberedIndex

        buttonId : Maybe Id
        buttonId =
            Just
                ("buttonLike"
                    ++ toString songRememberedIndex
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
    let
        buttonIdView : List (Attribute msg)
        buttonIdView =
            case buttonId of
                Nothing ->
                    []

                Just buttonId ->
                    [ id buttonId ]

        display : Display
        display =
            case buttonId of
                Nothing ->
                    "inline-block"

                Just buttonId ->
                    if
                        String.startsWith "buttonComment" buttonId
                            && not showCommentButtons
                    then
                        "none"
                    else
                        "inline-block"
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
    buttonMy buttonId hoverText SongsLatestFewRefreshHand


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
