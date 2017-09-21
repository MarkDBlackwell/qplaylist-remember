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


module ViewUtilities
    exposing
        ( goldenRatio
        , htmlNodeNull
        , showCommentButtons
        , songGroup2String
        )

import Html
    exposing
        ( Html
        , text
        )
import Message
    exposing
        ( Msg
        )
import ViewType
    exposing
        ( SongGroup
            ( Played
            , Remembered
            )
        )


-- VIEW


goldenRatio : Float
goldenRatio =
    --See:
    --https://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344
    0.6180339887498949


htmlNodeNull : Html Msg
htmlNodeNull =
    text ""


showCommentButtons : Bool
showCommentButtons =
    --TODO: Possibly inject.
    True


songGroup2String : SongGroup -> String
songGroup2String group =
    case group of
        Played ->
            "played"

        Remembered ->
            "remembered"
