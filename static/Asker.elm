module Asker (Action, Model, init, update, view) where

import Html exposing (Html, div, button, text, a)
import Effects exposing (Effects)
import TwitterTypes exposing (Tweet, User, Answer, Question)
import Array

type Model = Asking Question
           | Answered Answer

type Action = ShowQuestion Question
            | ShowAnswer Answer

init : Question -> (Model, Effects Action)
init question =
    (Asking question, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model =
    case model of

        Asking question ->
            div [] ([ (text question.tweet.text) ] ++
                    ( question.choices
                    |> Array.toList
                    |> List.map (.screenName) 
                    |> List.map (\screenName ->
                        div [] [ text screenName ]))
                   )

        Answered answer ->
            div [] [ (text answer.question.tweet.text)
                   , (text answer.answer.screenName)
                   ]


update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of

        ShowQuestion question ->
            (Asking question, Effects.none)

        ShowAnswer answer ->
            (Answered answer, Effects.none)