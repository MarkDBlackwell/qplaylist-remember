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
        , buttonLatest
        , buttonLike
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
    if Latest == songGroup then
        htmlNodeNull
    else
        buttonCommentView buttonIdMaybe hoverText buttonAction showCommentButtons


buttonCommentView : IdMaybe -> HoverText -> Msg -> ShowCommentButtons -> Html Msg
buttonCommentView buttonIdMaybe hoverText action showCommentButtons =
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
            maybeMapWithDefault default nonePossibly buttonIdMaybe
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


buttonForgetRemember : SongGroup -> SongsLatestOrRememberedIndex -> Html Msg
buttonForgetRemember songGroup songsLatestOrRememberedIndex =
    let
        buttonAction : Msg
        buttonAction =
            case songGroup of
                Latest ->
                    SongRememberHand songsLatestOrRememberedIndex

                Remembered ->
                    SongForgetHand songsLatestOrRememberedIndex

        buttonIdMaybe : IdMaybe
        buttonIdMaybe =
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
    buttonView buttonIdMaybe hoverText buttonAction


buttonLike : SongGroup -> SongsRememberedIndex -> Html Msg
buttonLike songGroup songsRememberedIndex =
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
    case songGroup of
        Latest ->
            htmlNodeNull

        Remembered ->
            buttonView buttonIdMaybe hoverText buttonAction


buttonView : IdMaybe -> HoverText -> Msg -> Html Msg
buttonView buttonIdMaybe hoverText action =
    buttonCommentView buttonIdMaybe hoverText action False


buttonLatest : Html Msg
buttonLatest =
    let
        buttonIdMaybe : IdMaybe
        buttonIdMaybe =
            Just "refresh"

        hoverText : HoverText
        hoverText =
            "Refresh the latest few songs"
    in
    buttonView buttonIdMaybe hoverText SongsLatestRefreshHand


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
    buttonView buttonIdMaybe hoverText PageMorphHand
