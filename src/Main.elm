module Main exposing (..)

import Array exposing (Array)
import Char
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random exposing (Generator)


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


type alias Chord =
    { name : String
    , interval : Int
    }



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



-- Chord definitions


thirdChord : Chord
thirdChord =
    { name = "major third", interval = 4 }


chords : Array Chord
chords =
    Array.fromList
        [ thirdChord
        , { name = "fifth", interval = 7 }
        , { name = "minor seventh", interval = 10 }
        ]


chordsNumber : Int
chordsNumber =
    Array.length chords


chordAt : Int -> Chord
chordAt idx =
    let
        elem =
            Array.get idx chords
    in
        case elem of
            Just chord ->
                chord

            Nothing ->
                thirdChord


type alias Model =
    { rootNote : Note
    , chord : Chord
    , response : Note
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
    ( Model "" thirdChord "" Pending "", randomQuiz )



-- UPDATE


type Msg
    = ResetQuiz
    | NewQuiz ( Note, Chord )
    | NewResponse Note
    | Check


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ResetQuiz ->
            ( { model | result = Pending, response = "" }, randomQuiz )

        NewQuiz ( newRoot, newChord ) ->
            ( { model | rootNote = newRoot, chord = newChord }, Cmd.none )

        NewResponse response ->
            ( { model | response = response }, Cmd.none )

        Check ->
            let
                ( result, solution ) =
                    checkResponse model.rootNote model.chord model.response
            in
                ( { model | result = result, solution = solution }, Cmd.none )



-- Take a random root from [A..G] chars and a random chord question


randomQuiz : Cmd Msg
randomQuiz =
    let
        gen =
            Random.pair randomRoot randomChord
    in
        Random.generate NewQuiz gen


randomRoot : Generator Note
randomRoot =
    Random.map (\n -> String.fromChar <| Char.fromCode (n + 65)) (Random.int 0 6)


randomChord : Generator Chord
randomChord =
    Random.map (\n -> chordAt n) (Random.int 0 chordsNumber)


getSolution : Note -> Chord -> ( Int, Note )
getSolution rootNote chord =
    let
        rootIndex =
            indexForNote rootNote chromaticScale

        solutionIndex =
            (rootIndex + chord.interval) % chromaticLength

        solution =
            noteForIndex solutionIndex chromaticScale
    in
        ( solutionIndex, solution )


checkResponse : Note -> Chord -> Note -> ( Result, Note )
checkResponse rootNote chord response =
    let
        ( solutionIndex, solution ) =
            getSolution rootNote chord

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
    div []
        [ div [ class "columns" ]
            [ h1 [ style [ centered ], class "column title" ] [ text "Chord Quiz" ]
            ]
        , div [ class "columns" ]
            [ div [ class "column is-8 is-offset-2" ]
                [ displayQuiz model
                ]
            ]
        ]


displayQuiz : Model -> Html Msg
displayQuiz model =
    section [ class "box" ]
        [ div [ class "columns" ]
            [ div [ class "column is-half has-text-right" ]
                [ h2 [ class "title is-4" ]
                    [ text <| "What is the " ++ model.chord.name ++ " of " ++ model.rootNote ++ " ? "
                    ]
                ]
            , div [ class "column is-3 control" ]
                [ input [ type_ "text", placeholder "response", onInput NewResponse, class "input" ] []
                ]
            ]
        , div [ class "columns" ]
            [ div [ class "column is-2 is-offset-4" ]
                [ button [ onClick Check, class "button is-primary" ] [ text "Check" ]
                ]
            , div [ class "column is-2" ]
                [ button [ onClick ResetQuiz, class "button" ] [ text "New Quiz" ]
                ]
            ]
        , div [ class "columns" ]
            [ displayAnswer model.result model.solution
            ]
        ]


displayAnswer : Result -> Note -> Html Msg
displayAnswer result solution =
    let
        ( msg, cls ) =
            case result of
                Good ->
                    ( "Good! You know your scale!", "is-success" )

                Bad ->
                    ( "Bad! Solution is " ++ solution, "is-danger" )

                Invalid ->
                    ( "This is not a note!", "is-warning" )

                Pending ->
                    ( "Write your response and check", "" )
    in
        div [ class (" column is-half is-offset-3 notification " ++ cls) ]
            [ text msg
            ]



-- Style


type alias CssDef =
    ( String, String )


centered : CssDef
centered =
    ( "text-align", "center" )
