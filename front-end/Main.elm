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


module Main exposing (main)

import Debug exposing (log)
import Dom
    exposing
        ( Id
        , focus
        )
import Html
    exposing
        ( Attribute
        , Html
        , a
        , button
        , div
        , em
        , hr
        , input
        , main_
        , p
        , section
        , span
        , text
        )
import Html.Attributes
    exposing
        ( autocomplete
        , class
        , href
        , id
        , placeholder
        , required
        , style
        , target
        , title
        , type_
        )
import Html.Events
    exposing
        ( onClick
        , onInput
        )
import Http
    exposing
        ( Error
        , Request
        , getString
        , send
        )
import Json.Decode
    exposing
        ( Decoder
        , decodeString
        , field
        , list
        , map
        , map4
        , string
        )
import Task
    exposing
        ( attempt
        , perform
        , succeed
        )


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type alias AlertMessage =
    String


type alias Artist =
    String


type alias AwaitingServerResponse =
    Bool


type alias LikeOrCommentText =
    String


type alias LikedOrCommented =
    Bool


type alias Model =
    { alertMessage : AlertMessage
    , awaitingServerResponse : AwaitingServerResponse
    , likeOrCommentText : LikeOrCommentText
    , pageIsExpanded : PageIsExpanded
    , processingComment : ProcessingComment
    , songRememberedCommentingIndex : Maybe SongRememberedCommentingIndex
    , songsLatestFew : SongsLatestFew
    , songsRemembered : SongsRemembered
    }


type alias PageIsExpanded =
    Bool


type alias ProcessingComment =
    Bool


type alias SongLatestFew =
    --Keep order (for JSON decoding):
    { artist : Artist
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongRemembered =
    { artist : Artist
    , likedOrCommented : LikedOrCommented
    , time : Time
    , timeStamp : TimeStamp
    , title : Title
    }


type alias SongRememberedCommentingIndex =
    Int


type alias SongsLatestFew =
    List SongLatestFew


type alias SongsRemembered =
    List SongRemembered


alertMessageInit : AlertMessage
alertMessageInit =
    ""


awaitingServerResponseInit : AwaitingServerResponse
awaitingServerResponseInit =
    False


likeOrCommentTextInit : LikeOrCommentText
likeOrCommentTextInit =
    ""


pageIsExpandedInit : PageIsExpanded
pageIsExpandedInit =
    False


processingCommentInit : ProcessingComment
processingCommentInit =
    False


songRememberedCommentingIndexInit : Maybe SongRememberedCommentingIndex
songRememberedCommentingIndexInit =
    Nothing


songsLatestFewInit : SongsLatestFew
songsLatestFewInit =
    []


songsRememberedInit : SongsRemembered
songsRememberedInit =
    []


init : ( Model, Cmd msg )
init =
    ( Model alertMessageInit awaitingServerResponseInit likeOrCommentTextInit pageIsExpandedInit processingCommentInit songRememberedCommentingIndexInit songsLatestFewInit songsRememberedInit
    , Cmd.none
    )



-- UPDATE


type alias DecodeErrorMessageText =
    String


type alias HttpErrorMessageText =
    String


type alias HttpRequestText =
    String


type alias HttpResponseText =
    String


type alias QueryBeforeList =
    --See:
    --https://github.com/elm-lang/url
    --https://tools.ietf.org/html/rfc3986
    --If joined, then comprises a URI's scheme, authority, and path:
    List UriText


type alias QueryPair =
    ( UriText, UriText )


type alias QueryPairs =
    List QueryPair


type alias SongLatestFewIndex =
    Int


type alias SongRememberedIndex =
    Int


type alias SongsLatestFewTagged =
    { latestFew : SongsLatestFew }


type alias Time =
    String


type alias TimeStamp =
    String


type alias Title =
    String


type alias UriText =
    String


type Msg
    = BuySongAnchorProcess
    | CommentInputCancel
    | CommentInputOk
    | CommentInputSetUp SongRememberedIndex
    | CommentInputTextChangeCapture LikeOrCommentText
    | CommentResponse (Result Error HttpResponseText)
    | FocusResult (Result Dom.Error ())
    | FocusSet Id
    | LikeButtonProcess SongRememberedIndex
    | LikeRequest
    | LikeResponse (Result Error HttpResponseText)
    | PageMorph
    | SongForget SongRememberedIndex
    | SongRemember SongLatestFewIndex
    | SongsLatestFewRefresh
    | SongsLatestFewResponse (Result Error HttpResponseText)


decodeSongsLatestFew : HttpResponseText -> SongsLatestFew
decodeSongsLatestFew jsonRawText =
    --See:
    --https://medium.com/@eeue56/json-decoding-in-elm-is-still-difficult-cad2d1fb39ae
    --http://eeue56.github.io/json-to-elm/
    --For decoding JSON:
    let
        decodeSong : Decoder SongLatestFew
        decodeSong =
            map4 SongLatestFew
                (field "artist" string)
                (field "time" string)
                (field "timeStamp" string)
                (field "title" string)

        tagged2Record : Decoder SongsLatestFewTagged
        tagged2Record =
            map SongsLatestFewTagged
                (field "latestFive" (list decodeSong))

        tryRecord : Result DecodeErrorMessageText SongsLatestFewTagged
        tryRecord =
            decodeString tagged2Record jsonRawText
    in
    case tryRecord of
        Err _ ->
            []

        Ok record ->
            record.latestFew


focusSet : Id -> Cmd Msg
focusSet id =
    msg2Cmd (succeed (FocusSet id))


msg2Cmd : Task.Task Never msg -> Cmd msg
msg2Cmd msg =
    --See:
    --https://github.com/billstclair/elm-dynamodb/blob/7ac30d60b98fbe7ea253be13f5f9df4d9c661b92/src/DynamoBackend.elm
    --For wrapping a message as a Cmd:
    Task.perform identity msg


relative : QueryBeforeList -> QueryPairs -> UriText
relative queryBeforeList queryPairs =
    --See:
    --https://github.com/elm-lang/http/issues/10
    --https://github.com/elm-lang/url
    --https://github.com/evancz/elm-http
    --http://package.elm-lang.org/packages/elm-lang/http/latest
    --TODO: When elm-lang/url is updated to contain 'relative', replace this code:
    let
        escapeAll : UriText -> UriText
        escapeAll string =
            --See:
            --http://package.elm-lang.org/packages/elm-lang/http/latest/Http
            --TODO: Possibly, use Http.encodeUri instead:
            escapeHashes (escapeEqualsSigns (escapeAmpersands string))

        escapeAmpersands : UriText -> UriText
        escapeAmpersands string =
            String.join
                "%26"
                (String.split "&" string)

        escapeEqualsSigns : UriText -> UriText
        escapeEqualsSigns string =
            String.join
                "%3D"
                (String.split "=" string)

        escapeHashes : UriText -> UriText
        escapeHashes string =
            String.join
                "%23"
                (String.split "#" string)

        query : UriText
        query =
            String.join
                "&"
                (List.map queryPairJoin queryPairs)

        queryBefore : UriText
        queryBefore =
            String.join
                "/"
                queryBeforeList

        queryPairJoin : QueryPair -> UriText
        queryPairJoin ( name, value ) =
            String.join
                "="
                [ name
                , escapeAll value
                ]
    in
    queryBefore ++ "?" ++ query


songLatestFew2Remembered : SongLatestFew -> SongRemembered
songLatestFew2Remembered song =
    { artist = song.artist
    , likedOrCommented = False
    , time = song.time
    , timeStamp = song.timeStamp
    , title = song.title
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        alertMessageSuffix : AlertMessage -> AlertMessage
        alertMessageSuffix thing =
            " (while attempting to send "
                ++ thing
                ++ " to server)"

        focusInputPossibly : Cmd Msg
        focusInputPossibly =
            if model.songRememberedCommentingIndex == songRememberedCommentingIndexInit then
                Cmd.none
            else
                focusSet "input"

        httpErrorMessageText : Error -> HttpErrorMessageText
        httpErrorMessageText httpError =
            let
                prefix : HttpErrorMessageText
                prefix =
                    "HttpError"
            in
            case httpError of
                Http.BadPayload debuggingText httpResponseText ->
                    log (prefix ++ ": BadPayload") debuggingText

                Http.BadStatus httpResponseText ->
                    log prefix "BadStatus"

                Http.BadUrl uriText ->
                    log (prefix ++ ": BadUrl") uriText

                Http.NetworkError ->
                    log prefix "NetworkError"

                Http.Timeout ->
                    log prefix "Timeout"

        likeOrCommentRequestUriText : UriText
        likeOrCommentRequestUriText =
            let
                artistTimeTitle : UriText
                artistTimeTitle =
                    case index of
                        Nothing ->
                            ""

                        Just _ ->
                            case songSelected of
                                Nothing ->
                                    ""

                                Just songSelected ->
                                    songSelected.time
                                        ++ " "
                                        ++ songSelected.artist
                                        ++ ": "
                                        ++ songSelected.title

                basename : UriText
                basename =
                    "append.php"

                index : Maybe SongRememberedIndex
                index =
                    model.songRememberedCommentingIndex

                songSelected : Maybe SongRemembered
                songSelected =
                    case index of
                        Nothing ->
                            Nothing

                        Just index ->
                            List.head (List.drop index model.songsRemembered)

                timeStamp : UriText
                timeStamp =
                    case index of
                        Nothing ->
                            ""

                        Just _ ->
                            case songSelected of
                                Nothing ->
                                    ""

                                Just song ->
                                    song.timeStamp
            in
            relative
                [ basename ]
                [ ( "timestamp", timeStamp )
                , ( "song", artistTimeTitle )
                , ( "comment", model.likeOrCommentText )
                ]

        likeOrCommentResponse : String -> ( Model, Cmd Msg )
        likeOrCommentResponse appendLikeOrCommentJson =
            let
                --Keep for console logging:
                a : String
                a =
                    logResponseOk appendLikeOrCommentJson

                sharedShow : SongsRemembered
                sharedShow =
                    List.indexedMap sharedShowSong model.songsRemembered

                sharedShowSong : SongRememberedIndex -> SongRemembered -> SongRemembered
                sharedShowSong index song =
                    if Just index == model.songRememberedCommentingIndex then
                        { song
                            | likedOrCommented = True
                        }
                    else
                        song
            in
            ( { model
                | alertMessage = alertMessageInit
                , awaitingServerResponse = awaitingServerResponseInit
                , likeOrCommentText = likeOrCommentTextInit
                , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                , songsRemembered = sharedShow
              }
            , Cmd.none
            )

        likingOrCommenting : Bool
        likingOrCommenting =
            model.songRememberedCommentingIndex /= songRememberedCommentingIndexInit

        logResponseOk : String -> String
        logResponseOk string =
            --log "Ok response" string
            log "Response" "Ok"
    in
    case msg of
        BuySongAnchorProcess ->
            ( model
            , focusInputPossibly
            )

        CommentInputCancel ->
            ( { model
                | alertMessage = alertMessageInit
                , likeOrCommentText = likeOrCommentTextInit
                , processingComment = processingCommentInit
                , songRememberedCommentingIndex = songRememberedCommentingIndexInit
              }
            , Cmd.none
            )

        CommentInputOk ->
            let
                commentRequest : Cmd Msg
                commentRequest =
                    send CommentResponse (getString (log "Request" likeOrCommentRequestUriText))
            in
            if String.isEmpty model.likeOrCommentText then
                ( { model
                    | alertMessage = alertMessageInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly
                )
            else
                ( { model
                    | awaitingServerResponse = True
                  }
                , Cmd.batch [ focusInputPossibly, commentRequest ]
                )

        CommentInputSetUp songRememberedIndex ->
            if likingOrCommenting then
                ( { model
                    | alertMessage = alertMessageInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly
                )
            else
                case model.songRememberedCommentingIndex of
                    Just _ ->
                        ( model
                          --, focusSet "refresh"
                        , focusInputPossibly
                        )

                    songRememberedCommentingIndexInit ->
                        ( { model
                            | processingComment = True
                            , songRememberedCommentingIndex = Just songRememberedIndex
                          }
                          --'focusInputPossibly' doesn't work, here:
                        , focusSet "input"
                        )

        CommentInputTextChangeCapture text ->
            ( { model
                | alertMessage = alertMessageInit
                , awaitingServerResponse = awaitingServerResponseInit
                , likeOrCommentText = text
              }
            , Cmd.none
            )

        CommentResponse (Err httpError) ->
            let
                alertMessageNew : AlertMessage
                alertMessageNew =
                    httpErrorMessageText httpError ++ alertMessageSuffix "comment"
            in
            ( { model
                | alertMessage = alertMessageNew
              }
            , focusInputPossibly
            )

        CommentResponse (Ok appendCommentJson) ->
            likeOrCommentResponse appendCommentJson

        FocusResult _ ->
            ( model
            , Cmd.none
            )

        FocusSet id ->
            --See:
            --https://www.reddit.com/r/elm/comments/53y6s4/focus_on_input_box_after_clicking_button/
            --https://stackoverflow.com/a/39419640/1136063
            ( model
            , attempt FocusResult (focus id)
            )

        LikeButtonProcess songRememberedIndex ->
            let
                likeText : LikeOrCommentText
                likeText =
                    "Loved it!"
            in
            if likingOrCommenting then
                if model.processingComment then
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                      }
                    , focusInputPossibly
                    )
                else
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                      }
                    , msg2Cmd (succeed (LikeButtonProcess songRememberedIndex))
                    )
            else
                case model.songRememberedCommentingIndex of
                    Just _ ->
                        ( { model
                            | alertMessage = alertMessageInit
                            , awaitingServerResponse = awaitingServerResponseInit
                          }
                        , focusInputPossibly
                        )

                    songRememberedCommentingIndexInit ->
                        ( { model
                            | alertMessage = alertMessageInit
                            , awaitingServerResponse = True
                            , likeOrCommentText = likeText
                            , songRememberedCommentingIndex = Just songRememberedIndex
                          }
                        , msg2Cmd (succeed LikeRequest)
                        )

        LikeRequest ->
            ( model
            , send LikeResponse (getString (log "Request" likeOrCommentRequestUriText))
            )

        LikeResponse (Err httpError) ->
            let
                alertMessageNew : AlertMessage
                alertMessageNew =
                    httpErrorMessageText httpError ++ alertMessageSuffix "Like"
            in
            ( { model
                | alertMessage = alertMessageNew
              }
            , Cmd.none
            )

        LikeResponse (Ok appendLikeJson) ->
            likeOrCommentResponse appendLikeJson

        PageMorph ->
            let
                pageIsExpandedNew : PageIsExpanded
                pageIsExpandedNew =
                    if
                        List.isEmpty model.songsLatestFew
                            && List.isEmpty model.songsRemembered
                    then
                        model.pageIsExpanded
                    else
                        not model.pageIsExpanded
            in
            if likingOrCommenting then
                ( { model
                    | alertMessage = alertMessageInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly
                )
            else
                ( { model
                    | alertMessage = alertMessageInit
                    , pageIsExpanded = pageIsExpandedNew
                  }
                , focusInputPossibly
                )

        SongForget songRememberedIndex ->
            let
                songsRememberedWithoutOne : SongsRemembered
                songsRememberedWithoutOne =
                    List.take songRememberedIndex model.songsRemembered
                        ++ List.drop (songRememberedIndex + 1) model.songsRemembered
            in
            if likingOrCommenting then
                if model.processingComment then
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                      }
                    , focusInputPossibly
                    )
                else if model.songRememberedCommentingIndex == Just songRememberedIndex then
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                      }
                    , Cmd.none
                    )
                else
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                      }
                    , msg2Cmd (succeed (SongForget songRememberedIndex))
                    )
            else
                ( { model
                    | songsRemembered = songsRememberedWithoutOne
                  }
                , focusSet "refresh"
                )

        SongRemember songLatestFewIndex ->
            let
                songClean : SongRemembered -> SongRemembered
                songClean song =
                    { song | likedOrCommented = False }

                songDiffers : SongRemembered -> Bool
                songDiffers song =
                    case songSelected of
                        Nothing ->
                            True

                        Just songSelected ->
                            songClean (songLatestFew2Remembered songSelected) /= songClean song

                songSelected : Maybe SongLatestFew
                songSelected =
                    List.head (List.drop songLatestFewIndex model.songsLatestFew)

                songsDifferent : SongsRemembered
                songsDifferent =
                    if Nothing == songSelected then
                        model.songsRemembered
                    else
                        List.filter songDiffers model.songsRemembered

                songsRememberedCleaned : SongsRemembered
                songsRememberedCleaned =
                    List.map songClean model.songsRemembered

                songsRememberedNew : SongsRemembered
                songsRememberedNew =
                    case songSelected of
                        Nothing ->
                            model.songsRemembered

                        Just songSelected ->
                            if
                                List.member
                                    (songClean (songLatestFew2Remembered songSelected))
                                    songsRememberedCleaned
                            then
                                model.songsRemembered
                            else
                                songsDifferent
                                    ++ [ songLatestFew2Remembered songSelected ]
            in
            if likingOrCommenting then
                if model.processingComment then
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                      }
                    , focusInputPossibly
                    )
                else
                    ( { model
                        | alertMessage = alertMessageInit
                        , awaitingServerResponse = awaitingServerResponseInit
                        , songRememberedCommentingIndex = songRememberedCommentingIndexInit
                      }
                    , Cmd.none
                    )
            else
                ( { model
                    | songsRemembered = songsRememberedNew
                  }
                , focusInputPossibly
                )

        SongsLatestFewRefresh ->
            let
                basename : UriText
                basename =
                    "LatestFive.json"

                request : Request HttpRequestText
                request =
                    getString (log "LatestFew" requestUriText)

                requestUriText : UriText
                requestUriText =
                    relative
                        [ ".."
                        , subUri
                        , basename
                        ]
                        []

                songsLatestFewRequest : Cmd Msg
                songsLatestFewRequest =
                    send SongsLatestFewResponse request

                subUri : UriText
                subUri =
                    "wtmdapp"
            in
            if likingOrCommenting then
                ( { model
                    | alertMessage = alertMessageInit
                    , awaitingServerResponse = awaitingServerResponseInit
                  }
                , focusInputPossibly
                )
            else
                ( model
                , Cmd.batch [ focusInputPossibly, songsLatestFewRequest ]
                )

        SongsLatestFewResponse (Err httpError) ->
            let
                alertMessageNew : AlertMessage
                alertMessageNew =
                    httpErrorMessageText httpError ++ suffix

                suffix : HttpErrorMessageText
                suffix =
                    " (while attempting to access the latest few songs)"
            in
            ( { model
                | alertMessage = alertMessageNew
              }
            , Cmd.none
            )

        SongsLatestFewResponse (Ok jsonRawText) ->
            let
                songsLatestFewNew : SongsLatestFew
                songsLatestFewNew =
                    decodeSongsLatestFew jsonRawText
            in
            ( { model
                | alertMessage = alertMessageInit
                , songsLatestFew = songsLatestFewNew
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        []



-- VIEW


type alias Display =
    String


type alias HoverText =
    String


type alias SongGroupLength =
    Int


type alias SongIndex =
    Int


type SongGroup
    = Played
    | Remembered


buttonComment : SongGroup -> SongRememberedIndex -> Html Msg
buttonComment group index =
    let
        buttonAction : Msg
        buttonAction =
            CommentInputSetUp index

        buttonId : Maybe Id
        buttonId =
            Just
                ("buttonComment"
                    ++ toString index
                )

        hoverText : HoverText
        hoverText =
            "Share a comment (with the DJ) about this song"
    in
    if Remembered == group then
        buttonMy buttonId hoverText buttonAction
    else
        htmlNodeNull


buttonForgetRemember : SongGroup -> SongIndex -> Html Msg
buttonForgetRemember group index =
    let
        buttonAction : Msg
        buttonAction =
            case group of
                Played ->
                    SongRemember index

                Remembered ->
                    SongForget index

        buttonId : Maybe Id
        buttonId =
            Just
                ("button"
                    ++ songGroup2String group
                    ++ toString index
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
buttonLike group index =
    let
        buttonAction : Msg
        buttonAction =
            LikeButtonProcess index

        buttonId : Maybe Id
        buttonId =
            Just
                ("buttonLike"
                    ++ toString index
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

        idMy : List (Attribute msg)
        idMy =
            case buttonId of
                Nothing ->
                    []

                Just buttonId ->
                    [ id buttonId ]
    in
    button
        ([ style [ ( "display", display ) ]
         , onClick action
         , title hoverText
         , type_ "button"
         ]
            ++ idMy
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
    buttonMy buttonId hoverText SongsLatestFewRefresh


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
    buttonMy buttonId hoverText PageMorph


buySongAnchor : SongRemembered -> Html Msg
buySongAnchor song =
    let
        fieldKeywords : UriText
        fieldKeywords =
            String.join
                "+"
                [ song.title
                , song.artist
                ]

        hoverText : HoverText
        hoverText =
            "See this song on Amazon (in new tab)"

        queryBeforeList : QueryBeforeList
        queryBeforeList =
            [ "http://www.amazon.com/s/ref=nb_sb_noss" ]

        queryPairs : QueryPairs
        queryPairs =
            [ ( "tag", "wtmdradio-20" )
            , ( "url", "search-alias=digital-music" )
            , ( "field-keywords"
              , fieldKeywords
              )
            ]

        uriText : UriText
        uriText =
            relative queryBeforeList queryPairs
    in
    a
        [ href uriText
        , onClick BuySongAnchorProcess
        , target "_blank"
        , title hoverText
        ]
        []


commentArea : Model -> SongRemembered -> Html Msg
commentArea model song =
    let
        statistics : String
        statistics =
            " – "
                ++ song.timeStamp
                ++ ": "
                ++ toString (String.length model.likeOrCommentText)

        hoverText : HoverText
        hoverText =
            "Type your (additional) comment here!"
    in
    section
        [ id "comment" ]
        [ p []
            [ text
                (song.artist
                    ++ ": "
                    ++ song.title
                    ++ " ("
                    ++ song.time
                    ++ ")"
                    ++ statistics
                )
            ]
        , input
            [ autocomplete False
            , id "input"
            , onInput CommentInputTextChangeCapture
            , placeholder hoverText
            , required True
            , title hoverText
            , type_ "text"
            ]
            []
        , buttonMy Nothing "Submit your comment" CommentInputOk
        , buttonMy Nothing "Cancel this comment" CommentInputCancel
        ]


commentAreaPossibly : Model -> Html Msg
commentAreaPossibly model =
    let
        songPossibly : SongRememberedIndex -> Maybe SongRemembered
        songPossibly index =
            List.head (List.drop index model.songsRemembered)
    in
    if not model.processingComment then
        htmlNodeNull
    else
        case model.songRememberedCommentingIndex of
            Just index ->
                case songPossibly index of
                    Nothing ->
                        htmlNodeNull

                    Just song ->
                        commentArea model song

            songRememberedCommentingIndexInit ->
                htmlNodeNull


groupAttributes : SongGroup -> List (Attribute msg)
groupAttributes group =
    [ class "songs-group"
    , id
        ("songs-"
            ++ songGroup2String group
        )
    ]


htmlNodeNull : Html Msg
htmlNodeNull =
    text ""


showCommentButtons : Bool
showCommentButtons =
    True


songGroup2String : SongGroup -> String
songGroup2String group =
    case group of
        Played ->
            "played"

        Remembered ->
            "remembered"


songView : Model -> SongGroup -> SongIndex -> SongRemembered -> Html Msg
songView model group index song =
    let
        lengthRemembered : SongGroupLength
        lengthRemembered =
            List.length model.songsRemembered

        likedOrCommentedIndicator : Html Msg
        likedOrCommentedIndicator =
            if song.likedOrCommented then
                em [ title likedOrCommentedIndicatorHoverText ]
                    []
            else
                htmlNodeNull

        likedOrCommentedIndicatorHoverText : HoverText
        likedOrCommentedIndicatorHoverText =
            "You've shared a 'Like'"
                ++ likedOrCommentedIndicatorHoverTextCommentButton
                ++ " about this song (with the DJ)"

        likedOrCommentedIndicatorHoverTextCommentButton : HoverText
        likedOrCommentedIndicatorHoverTextCommentButton =
            if showCommentButtons then
                " (or a comment)"
            else
                ""

        songAttributes : List (Attribute msg)
        songAttributes =
            if model.pageIsExpanded then
                []
            else
                [ styleCalc group lengthRemembered index ]
    in
    div
        songAttributes
        [ p []
            [ buttonForgetRemember group index
            , span []
                [ text song.time ]
            , buttonComment group index
            , buttonLike group index
            , likedOrCommentedIndicator
            , buySongAnchor song
            ]
        , p []
            [ text song.title ]
        , p []
            [ text song.artist ]
        ]


styleCalc : SongGroup -> SongGroupLength -> SongIndex -> Attribute msg
styleCalc group songGroupLength index =
    let
        backgroundColorStyling : List ( String, String )
        backgroundColorStyling =
            case group of
                Played ->
                    []

                Remembered ->
                    [ ( "background-color", backgroundColorValue ) ]

        backgroundColorValue : String
        backgroundColorValue =
            "hsl(0,"
                ++ toString (saturation * 100.0)
                ++ "%,50%"

        base : Float
        base =
            16.0

        fontSizeStyling : List ( String, String )
        fontSizeStyling =
            [ ( "font-size", fontSizeValue ) ]

        fontSizeValue : String
        fontSizeValue =
            toString (sizeFactor * base)
                ++ "px"

        goldenRatio : Float
        goldenRatio =
            --See:
            --https://en.wikipedia.org/w/index.php?title=Golden_ratio&oldid=790709344
            0.6180339887498949

        indexReversed : SongIndex
        indexReversed =
            songGroupLength - index - 1

        saturation : Float
        saturation =
            sizeFactor * 0.5

        sizeFactor : Float
        sizeFactor =
            case group of
                Played ->
                    goldenRatio ^ toFloat index

                Remembered ->
                    goldenRatio ^ toFloat indexReversed
    in
    style
        (backgroundColorStyling
            ++ fontSizeStyling
        )


view : Model -> Html Msg
view model =
    let
        alertArea : Html Msg
        alertArea =
            section
                [ id "alert" ]
                [ p []
                    [ text model.alertMessage ]
                ]

        songsLatestFew : List (Html Msg)
        songsLatestFew =
            List.indexedMap (songView model Played) songsLatestFew2Remembered

        songsLatestFew2Remembered : SongsRemembered
        songsLatestFew2Remembered =
            List.map songLatestFew2Remembered model.songsLatestFew

        songsRemembered : List (Html Msg)
        songsRemembered =
            List.indexedMap (songView model Remembered) model.songsRemembered
    in
    main_
        []
        [ alertArea
        , commentAreaPossibly model
        , section
            (groupAttributes Remembered)
            ([ p []
                [ buttonRemembered ]
             ]
                ++ songsRemembered
            )
        , hr [] []
        , section
            (groupAttributes Played)
            ([ p []
                [ buttonPlayed ]
             ]
                ++ songsLatestFew
            )
        ]
