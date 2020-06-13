{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Alert exposing
    ( actionDescriptionRecent
    , messageTextErrorHttpLogging
    , messageTextErrorHttpScreen
    , messageTextInit
    , messageTextRequestLikeOrComment
    , messageTextSend
    , messageTextServerAwaitingElmCycle
    )

import AlertType
    exposing
        ( ActionDescription
        , AlertMessageText
        , AlertMessageTextMaybe
        , DetailsText
        , LikeOrCommentName
        , PrefixSeparatorText
        )
import ElmCycle
import FocusUpdate
import Http
import ModelType
    exposing
        ( Model
        )



-- MODEL


messageTextInit : AlertMessageTextMaybe
messageTextInit =
    Nothing



-- UPDATE


actionDescriptionRecent : AlertMessageText
actionDescriptionRecent =
    "access the latest few songs"


errorHttpText : Http.Error -> ( AlertMessageText, AlertMessageText )
errorHttpText httpError =
    let
        prefix : PrefixSeparatorText
        prefix =
            "HttpError"
    in
    case httpError of
        Http.BadBody debuggingText ->
            ( prefix ++ prefixSeparator ++ "BadBody"
            , debuggingText
            )

        Http.BadStatus status ->
            ( prefixSeparator ++ "BadStatus"
            , String.fromInt status
            )

        Http.BadUrl urlText ->
            ( prefixSeparator ++ "BadUrl"
            , urlText
            )

        Http.NetworkError ->
            ( prefix
            , "NetworkError"
            )

        Http.Timeout ->
            ( prefix
            , "Timeout"
            )


messageTextErrorHttpLogging : Http.Error -> AlertMessageText
messageTextErrorHttpLogging httpError =
    httpError
        |> (errorHttpText >> Tuple.first)


messageTextErrorHttpScreen : Http.Error -> AlertMessageText
messageTextErrorHttpScreen httpError =
    httpError
        |> (errorHttpText >> Tuple.second)


messageTextRequestLikeOrComment : Http.Error -> LikeOrCommentName -> AlertMessageText
messageTextRequestLikeOrComment httpError likeOrCommentName =
    let
        prefix : String
        prefix =
            httpError
                |> messageTextErrorHttpScreen
    in
    [ " (while attempting to send your "
    , likeOrCommentName
    , ")"
    ]
        |> String.concat
        |> String.append prefix


messageTextSend : ActionDescription -> DetailsText -> AlertMessageText
messageTextSend actionDescription detailsText =
    [ "Unexpected error while attempting to "
    , actionDescription
    , prefixSeparator
    , detailsText
    ]
        |> String.concat


messageTextServerAwaitingElmCycle : Model -> ElmCycle.ElmCycle
messageTextServerAwaitingElmCycle model =
    ( { model
        | alertMessageText = Just "Awaiting server"
      }
    , FocusUpdate.cmdFocusInputPossibly model
    )


prefixSeparator : PrefixSeparatorText
prefixSeparator =
    ": "
