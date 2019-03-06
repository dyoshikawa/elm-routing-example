module Main exposing (Msg(..), main, update, view)

import Browser
import Html exposing (..)
import Html.Events exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { count = 0 }, Cmd.none )



-- MODEL


type alias Model =
    { count : Int }



--UPDATE


type Msg
    = Increment
    | Decrement


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            let
                newCount =
                    model.count + 1
            in
            ( { model | count = newCount }, Cmd.none )

        Decrement ->
            let
                newCount =
                    model.count - 1
            in
            ( { model | count = newCount }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ button [ onClick Decrement ] [ text "-" ]
        , div [] [ text (String.fromInt model.count) ]
        , button [ onClick Increment ] [ text "+" ]
        ]



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
