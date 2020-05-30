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
        , AlertMessageTextList
        , AlertMessageTextMaybe
        , DetailsText
        , LikeOrCommentName
        , PrefixSeparatorText
        )
import ElmCycle
import Http
import ModelType
    exposing
        ( Model
        )
import UpdateFocus



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

        Http.BadUrl uriText ->
            ( prefixSeparator ++ "BadUrl"
            , uriText
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
        |> errorHttpText
        |> Tuple.first


messageTextErrorHttpScreen : Http.Error -> AlertMessageText
messageTextErrorHttpScreen httpError =
    httpError
        |> errorHttpText
        |> Tuple.second


messageTextRequestLikeOrComment : Http.Error -> LikeOrCommentName -> AlertMessageText
messageTextRequestLikeOrComment httpError likeOrCommentName =
    (++)
        (messageTextErrorHttpScreen httpError)
        ([ " (while attempting to send your "
         , likeOrCommentName
         , ")"
         ]
            |> String.concat
        )


messageTextSend : ActionDescription -> DetailsText -> AlertMessageText
messageTextSend actionDescription detailsText =
    let
        unexpected : AlertMessageTextList -> AlertMessageText
        unexpected alertMessageTextList =
            (++)
                "Unexpected error "
                (alertMessageTextList
                    |> String.join prefixSeparator
                )
    in
    unexpected
        [ [ "while attempting to "
          , actionDescription
          ]
            |> String.concat
        , detailsText
        ]


messageTextServerAwaitingElmCycle : Model -> ElmCycle.ElmCycle
messageTextServerAwaitingElmCycle model =
    ( { model
        | alertMessageText = Just "Awaiting server"
      }
    , UpdateFocus.focusInputPossibly model
    )


prefixSeparator : PrefixSeparatorText
prefixSeparator =
    ": "
