module Main exposing (..)

import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }



-- MODEL


type alias Note =
    String


type alias Model =
    { rootNote : Note
    , result : Result
    , solution : Note
    }


type Result
    = Good
    | Bad
    | Invalid
    | Pending


init : ( Model, Cmd Msg )
init =
    ( Model "A" Pending "", Cmd.none )



-- UPDATE


type Msg
    = NewQuiz
    | NewRoot Note
    | GiveResponse Note


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewQuiz ->
            ( model, Random.generate NewRoot randomRootNote )

        NewRoot newRoot ->
            ( { model | rootNote = newRoot }, Cmd.none )

        GiveResponse response ->
            let
                ( result, solution ) =
                    checkResponse model.rootNote response
            in
                ( { model | result = result, solution = solution }, Cmd.none )


charOffsetForNote : Int
charOffsetForNote =
    65


randomRootNote : Random.Generator String
randomRootNote =
    Random.map (\n -> String.fromChar <| Char.fromCode (n + charOffsetForNote)) (Random.int 0 6)


checkResponse : Note -> Note -> ( Result, Note )
checkResponse rootNote response =
    ( Good, "" )



-- VIEW


view : Model -> Html Msg
view model =
    section [ style [ centered ] ]
        [ div []
            [ text <| "Quelle est la tierce majeure de " ++ model.rootNote ++ " ? "
            , input [ type_ "text", placeholder "Réponse", onInput GiveResponse ] []
            ]
        , div []
            [ displayAnswer model.result model.solution
            ]
        , button [ onClick NewQuiz ] [ text "New Quiz" ]
        ]


displayAnswer : Result -> Note -> Html Msg
displayAnswer result solution =
    case result of
        Good ->
            text "Bravo ! Vous connaissez votre gamme !"

        Bad ->
            text ("Perdu ! La solution est " ++ solution)

        Invalid ->
            text "Je ne reconnais pas cette note"

        Pending ->
            text ""



-- Style


type alias CssDef =
    ( String, String )


centered : CssDef
centered =
    ( "text-align", "center" )
