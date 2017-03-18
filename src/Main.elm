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


type alias Scale =
    List ( Int, Note )



-- chromaticScale == [(0, "A"), (1, "B"), ...]


chromaticScale : Scale
chromaticScale =
    List.indexedMap (,) [ "A", "A#", "B", "C", "C#", "D", "D#", "E", "F", "F#", "G", "G#" ]


chromaticLength : Int
chromaticLength =
    List.length chromaticScale


indexForNote : Note -> Scale -> Int
indexForNote note scale =
    let
        n =
            List.filter (\( _, n ) -> n == note) scale
    in
        case n of
            ( i, _ ) :: _ ->
                i

            _ ->
                chromaticLength


noteForIndex : Int -> Scale -> Note
noteForIndex index scale =
    let
        n =
            List.filter (\( i, _ ) -> i == index) scale
    in
        case n of
            ( _, note ) :: _ ->
                note

            _ ->
                "Unknown"


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
    ( Model "" Pending "", randomRoot )



-- UPDATE


type Msg
    = NewQuiz
    | NewRoot Note
    | GiveResponse Note


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewQuiz ->
            ( { model | result = Pending }, randomRoot )

        NewRoot newRoot ->
            ( { model | rootNote = newRoot }, Cmd.none )

        GiveResponse response ->
            let
                ( result, solution ) =
                    checkResponse model.rootNote response
            in
                ( { model | result = result, solution = solution }, Cmd.none )



-- Take a random root from [A..G] chars


randomRoot : Cmd Msg
randomRoot =
    let
        noteGen =
            Random.map (\n -> String.fromChar <| Char.fromCode (n + 65)) (Random.int 0 6)
    in
        Random.generate NewRoot noteGen


getSolution : Note -> ( Int, Note )
getSolution rootNote =
    let
        rootIndex =
            indexForNote rootNote chromaticScale

        solutionIndex =
            (rootIndex + 4) % chromaticLength

        solution =
            noteForIndex solutionIndex chromaticScale
    in
        ( solutionIndex, solution )


checkResponse : Note -> Note -> ( Result, Note )
checkResponse rootNote response =
    let
        ( solutionIndex, solution ) =
            getSolution rootNote

        responseIndex =
            indexForNote (String.toUpper response) chromaticScale

        result =
            if responseIndex == solutionIndex then
                Good
            else if responseIndex == chromaticLength then
                Invalid
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
            text "Ce n'est pas une note !"

        Pending ->
            text ""



-- Style


type alias CssDef =
    ( String, String )


centered : CssDef
centered =
    ( "text-align", "center" )
