module Main exposing (Model, Msg(..), init, main, update, view)

import Array
import Browser
import Browser.Dom
import Data exposing (dataList)
import Debug exposing (log)
import Html exposing (Html, div, h1, img, input, span, text)
import Html.Attributes exposing (class, id, placeholder, src, type_, value)
import Html.Events exposing (onInput)
import Task



---- MODEL ----


type WordState
    = Success
    | Error
    | Neutral


type alias Word =
    { value : String, state : WordState }


type alias Model =
    { txt : String
    , wordList : List Word
    , index : Int
    , offsetTop : Float
    }


init : ( Model, Cmd Msg )
init =
    ( { txt = ""
      , wordList = List.map (\word -> Word word Neutral) dataList
      , index = 0
      , offsetTop = 0
      }
    , Cmd.none
    )



---- UPDATE ---


getElementAtIndex : List a -> Int -> Maybe a
getElementAtIndex list idx =
    List.head (List.drop idx (List.take (idx + 1) list))


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
    if newTxt /= "nille är bäst " then
        -- ( { model | txt = "" }, Cmd.none )
        ( { model | txt = "", index = model.index + 1 }
        , Task.attempt GotCurrentElement (Browser.Dom.getElement "current")
        )

    else if String.endsWith " " newTxt then
        if newTxt == (getCurrentWord model ++ " ") then
            ( { model | txt = "", index = model.index + 1 }
            , Task.attempt GotCurrentElement (Browser.Dom.getElement "current")
            )

        else
            -- ( { model | txt = "" }
            ( { model | txt = "", index = model.index + 1 }
            , Cmd.none
            )

    else
        ( { model | txt = newTxt }, Cmd.none )


type Msg
    = TextChanged String
    | GotCurrentElement (Result Browser.Dom.Error Browser.Dom.Element)
    | NoOp


jumpToOffset : Float -> Float -> Cmd Msg
jumpToOffset y y2 =
    Browser.Dom.setViewportOf "word-box" 0 (y - y2)
        |> Task.onError (\_ -> Task.succeed ())
        |> Task.perform (\_ -> NoOp)



--     Browser.Dom.getElement "word-box"
--         |> Task.andThen
--             (\info ->
--                 Browser.Dom.setViewportOf "word-box" 0 (y - y2)
--             )
--         |> Task.onError (\_ -> Task.succeed ())
--         |> Task.perform (\_ -> NoOp)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TextChanged newTxt ->
            handleTextChanged newTxt model

        GotCurrentElement result ->
            case result of
                Ok r ->
                    ( { model | offsetTop = r.viewport.y }
                    , jumpToOffset r.element.y r.viewport.y
                    )

                Err _ ->
                    ( model, Cmd.none )

        NoOp ->
            ( model, Cmd.none )



---- VIEW ----


wordToSpan : Int -> Int -> Word -> Html Msg
wordToSpan index current word =
    if index == current then
        span [ class "current", id "current" ] [ text word.value ]

    else
        span [] [ text word.value ]


view : Model -> Html Msg
view model =
    div []
        [ img [ src "/logo.svg" ] []
        , h1 [] [ text "Your Elm App is working!" ]
        , div [ class "word-box", id "word-box" ] (List.indexedMap (wordToSpan model.index) model.wordList)
        , viewInput "text" "" model.txt TextChanged
        ]


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []



---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
