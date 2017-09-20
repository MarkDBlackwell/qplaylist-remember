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


module AlertMessage
    exposing
        ( AlertMessageText
        , DecodeErrorMessageText
        , alertMessageTextAwaitingServer
        , alertMessageTextInit
        , alertMessageTextUnexpectedError
        )

-- MODEL


type alias AlertMessageText =
    String


type alias DecodeErrorMessageText =
    String


alertMessageTextAwaitingServer : AlertMessageText
alertMessageTextAwaitingServer =
    "Awaiting server"


alertMessageTextInit : AlertMessageText
alertMessageTextInit =
    ""


alertMessageTextUnexpectedError : AlertMessageText -> DecodeErrorMessageText -> AlertMessageText
alertMessageTextUnexpectedError alertMessageText decodeErrorMessageText =
    "Unexpected error "
        ++ alertMessageText
        ++ ": "
        ++ decodeErrorMessageText
