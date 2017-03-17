module Main exposing (..)

import Char
import Html exposing (..)
import Html.Events exposing (..)
import Random


main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Model =
    { rootNote : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model "A", Cmd.none )



-- UPDATE


type Msg
    = Roll
    | NewFace String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Roll ->
            ( model, Random.generate NewFace randomRootNote )

        NewFace newFace ->
            ( Model newFace, Cmd.none )


randomRootNote =
    Random.map (\n -> String.fromChar <| Char.fromCode (n + 65)) (Random.int 0 6)



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h1 [] [ text model.rootNote ]
        , button [ onClick Roll ] [ text "Roll" ]
        ]
