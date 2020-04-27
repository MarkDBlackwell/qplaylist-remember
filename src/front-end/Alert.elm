{- Copyright (C) 2018 Mark D. Blackwell.
   All rights reserved.
   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
-}


module Alert exposing
    ( actionDescriptionRecent
    , alertMessageTextErrorHttpLogging
    , alertMessageTextErrorHttpScreen
    , alertMessageTextInit
    , alertMessageTextRequestLikeOrComment
    , alertMessageTextSend
    , alertMessageTextServerAwaitingElmCycle
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
    exposing
        ( ElmCycle
        )
import Http
    exposing
        ( Error
        )
import ModelType
    exposing
        ( Model
        )
import Tuple
    exposing
        ( first
        , second
        )
import UpdateFocus
    exposing
        ( focusInputPossibly
        )
import Utilities
    exposing
        ( prefixSeparator
        )



-- MODEL


alertMessageTextInit : AlertMessageTextMaybe
alertMessageTextInit =
    Nothing



-- UPDATE


actionDescriptionRecent : AlertMessageText
actionDescriptionRecent =
    "access the latest few songs"


alertMessageTextErrorHttpLogging : Error -> AlertMessageText
alertMessageTextErrorHttpLogging httpError =
    errorHttpText httpError
        |> first


alertMessageTextErrorHttpScreen : Error -> AlertMessageText
alertMessageTextErrorHttpScreen httpError =
    errorHttpText httpError
        |> second


alertMessageTextRequestLikeOrComment : Error -> LikeOrCommentName -> AlertMessageText
alertMessageTextRequestLikeOrComment httpError likeOrCommentName =
    (++)
        (alertMessageTextErrorHttpScreen httpError)
        (String.concat
            [ " (while attempting to send your "
            , likeOrCommentName
            , ")"
            ]
        )


alertMessageTextSend : ActionDescription -> DetailsText -> AlertMessageText
alertMessageTextSend actionDescription detailsText =
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
        [ String.concat
            [ "while attempting to "
            , actionDescription
            ]
        , detailsText
        ]


alertMessageTextServerAwaitingElmCycle : Model -> ElmCycle
alertMessageTextServerAwaitingElmCycle model =
    ( { model
        | alertMessageText = Just "Awaiting server"
      }
    , focusInputPossibly model
    )


errorHttpText : Error -> ( AlertMessageText, AlertMessageText )
errorHttpText httpError =
    let
        prefix : PrefixSeparatorText
        prefix =
            "HttpError"
    in
    case httpError of
        Http.BadPayload debuggingText httpResponseText ->
            ( prefix ++ prefixSeparator ++ "BadPayload"
            , debuggingText
            )

        Http.BadStatus httpResponseText ->
            ( prefixSeparator ++ "BadStatus"
            , String.fromInt httpResponseText.status
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
