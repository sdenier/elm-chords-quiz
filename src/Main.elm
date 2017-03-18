module Main exposing (..)

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


type alias Scale =
    List Note


majorScale : Scale
majorScale =
    [ "A", "B", "C", "D", "E", "F", "G" ]


fullScale : Scale
fullScale =
    [ "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#" ]


noteAt : Int -> Scale -> Note
noteAt index scale =
    case index of
        0 ->
            case List.head scale of
                Just note ->
                    note

                Nothing ->
                    "A"

        _ ->
            case (List.tail scale) of
                Just endScale ->
                    noteAt (index - 1) endScale

                Nothing ->
                    "A"


indexFor : Note -> Scale -> Int -> Int
indexFor note scale idx =
    case scale of
        n :: rest ->
            if n == note then
                idx
            else
                indexFor note rest (idx + 1)

        [] ->
            13


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
            ( { model | result = Pending }, Random.generate NewRoot randomNoteInMajorScale )

        NewRoot newRoot ->
            ( { model | rootNote = newRoot }, Cmd.none )

        GiveResponse response ->
            let
                ( result, solution ) =
                    checkResponse model.rootNote response
            in
                ( { model | result = result, solution = solution }, Cmd.none )


randomNoteInMajorScale : Random.Generator Note
randomNoteInMajorScale =
    Random.map (\n -> noteAt n majorScale) (Random.int 0 6)


checkResponse : Note -> Note -> ( Result, Note )
checkResponse rootNote response =
    let
        rootIndex =
            indexFor rootNote fullScale 0

        thirdIndex =
            (rootIndex + 4) % (List.length fullScale)

        solution =
            noteAt thirdIndex fullScale

        givenNote =
            String.toUpper response

        result =
            if givenNote == solution then
                Good
            else
                Bad
    in
        ( result, solution )



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
