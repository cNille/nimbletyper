module Main exposing (Model, Msg(..), init, main, update, view)

import Browser
import Browser.Dom
import Data exposing (dataList)
import Debug exposing (log)
import Html exposing (Html, button, div, h1, hr, img, input, span, text)
import Html.Attributes exposing (class, disabled, id, placeholder, src, type_, value)
import Html.Events exposing (onClick, onInput)
import Task
import Time



---- UTIL ----


getElementAtIndex : List a -> Int -> Maybe a
getElementAtIndex list idx =
    List.head (List.drop idx (List.take (idx + 1) list))



---- MODEL ----


type WordState
    = Success
    | Error
    | Neutral


type alias Word =
    { value : String
    , state : WordState
    , show : Bool
    }


type alias Model =
    { txt : String
    , wordList : List Word
    , index : Int
    , offset : Float
    , lineDone : Bool
    , timeLeft : Int
    , gameStarted : Bool
    }


init : ( Model, Cmd Msg )
init =
    ( { txt = ""
      , wordList = List.map (\word -> Word word Neutral True) dataList
      , index = 0
      , offset = 0
      , lineDone = False
      , timeLeft = 60
      , gameStarted = False
      }
    , Cmd.none
    )



---- UPDATE ---


type Msg
    = TextChanged String
    | GotCurrentElement (Result Browser.Dom.Error Browser.Dom.Element)
    | StartGame
    | Tick Time.Posix
    | NoOp


jumpToOffset : Float -> Float -> Cmd Msg
jumpToOffset y y2 =
    Browser.Dom.setViewportOf "word-box" 0 (y - y2)
        |> Task.onError (\_ -> Task.succeed ())
        |> Task.perform (\_ -> NoOp)


getCurrentWord : Model -> String
getCurrentWord model =
    case
        getElementAtIndex model.wordList model.index
    of
        Nothing ->
            ""

        Just word ->
            word.value


handleTextChanged : String -> Model -> ( Model, Cmd Msg )
handleTextChanged newTxt model =
    if not model.gameStarted then
        ( { model | txt = "" }, Cmd.none )

    else if String.endsWith " " newTxt then
        if newTxt == (getCurrentWord model ++ " ") then
            ( { model | txt = "", index = model.index + 1, lineDone = False }
            , Task.attempt GotCurrentElement (Browser.Dom.getElement "current")
            )

        else
            ( { model | txt = "" }
            , Cmd.none
            )

    else
        ( { model | txt = newTxt }, Cmd.none )


hideOldWord : Int -> Int -> Word -> Word
hideOldWord index i w =
    if i < index then
        { w | show = False }

    else
        w


hideOldWords : Model -> List Word
hideOldWords model =
    List.indexedMap
        (hideOldWord model.index)
        model.wordList


offsetWordListIfNewLine : Model -> Browser.Dom.Element -> ( Model, Cmd Msg )
offsetWordListIfNewLine model r =
    let
        lineDone =
            r.element.y /= model.offset

        offset =
            r.element.y
    in
    if lineDone && model.index > 2 then
        ( { model | offset = offset, lineDone = lineDone, wordList = hideOldWords model }, Cmd.none )

    else
        ( { model | offset = offset, lineDone = lineDone }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextChanged newTxt ->
            handleTextChanged newTxt model

        GotCurrentElement result ->
            case result of
                Ok r ->
                    offsetWordListIfNewLine model r

                Err _ ->
                    ( model, Cmd.none )

        StartGame ->
            ( { model | gameStarted = True }, Task.attempt (\_ -> NoOp) (Browser.Dom.focus "inputfield") )

        Tick _ ->
            let
                timeLeft =
                    model.timeLeft - 1
            in
            ( { model | timeLeft = timeLeft, gameStarted = timeLeft > 0 }
            , Cmd.none
            )

        NoOp ->
            ( model, Cmd.none )



---- SUBSCRIPTIONS ----


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.gameStarted && model.timeLeft >= 0 then
        Time.every 1000 Tick

    else
        Sub.none



---- VIEW ----


wordToSpan : String -> Int -> Int -> Word -> Html Msg
wordToSpan currInput index current word =
    if word.show then
        if index == current then
            span
                [ class
                    (if String.startsWith currInput word.value then
                        "current"

                     else
                        "current wrong"
                    )
                , id "current"
                ]
                [ text word.value ]

        else
            span [] [ text word.value ]

    else
        text ""


getWordsFromModel : Model -> List (Html Msg)
getWordsFromModel model =
    List.indexedMap (wordToSpan model.txt model.index) model.wordList


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , div [ class "word-box", id "word-box" ] (getWordsFromModel model)
        , viewInput "text" "" model.txt "inputfield" TextChanged (not model.gameStarted)
        , hr [] []
        , button [ onClick StartGame, disabled model.gameStarted ] [ Html.text "Start game" ]
        , Html.text (String.fromInt model.timeLeft ++ " seconds left")
        ]


viewInput : String -> String -> String -> String -> (String -> msg) -> Bool -> Html msg
viewInput t p v i toMsg d =
    input [ type_ t, placeholder p, value v, onInput toMsg, id i, disabled d ] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
