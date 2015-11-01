module Asker (Action, Model, ChoiceState(..), getResult, init, update, view) where

import Html exposing (Html, Attribute, div, text, a)
import Effects exposing (Effects)
import TwitterTypes exposing (Tweet, User, Answer, Question)
import Array exposing (Array)
import Signal
import Html.Events exposing (onClick)
import Html.Attributes exposing (style, class)

type Model = Asking Question
           | Answered Answer

type Action = ShowQuestion Question
            | ShowAnswer Answer
            | Reply User

type ChoiceState = Correct
                 | Wrong
                 | NotSelected

getResult : Model -> Maybe Answer
getResult model =
    case model of
        Answered answer -> Just answer
        otherwise       -> Nothing

init : Question -> (Model, Effects Action)
init question =
    (Asking question, Effects.none)

view : Signal.Address Action -> Model -> Html
view address model =
    div [class "asker"] [
    case model of

        Asking question ->
            div [] [ (div [class "question-text"] [text question.tweet.text])
                   , (div [class "choices"] (buttons question.choices (choiceAttributes address)))
                   ]

        Answered answer ->
            div [] [ (div [class "question-text"] [text answer.question.tweet.text])
                   , (div [class "choices"] (buttons answer.question.choices
                        (answerAttributes answer.answer answer.question.tweet.user)))
                   ]
    ]

buttons : Array User -> (User -> List Attribute) -> List Html
buttons choices getAttribute =
    choices
    |> Array.map (\user ->
        div (getAttribute user) [ text user.screenName ])
    |> Array.toList

choiceAttributes : Signal.Address Action -> User -> List Attribute
choiceAttributes address user =
    [ onClick address (Reply user) ]
    -- Add some style here

answerAttributes : User -> User -> User -> List Attribute
answerAttributes answered correct current =
    let isSelected = current.screenName == answered.screenName
        isCorrect  = current.screenName == correct.screenName
    in [ answerStyleAttributes (getChoiceState isSelected isCorrect) ]

answerStyleAttributes : ChoiceState -> Attribute
answerStyleAttributes state =
    style [ (
        case state of
            Correct -> ("color", "green")
            Wrong -> ("color", "red")
            NotSelected -> ("","")
    )]

getChoiceState : Bool -> Bool -> ChoiceState
getChoiceState selected correct =
    case (selected, correct) of
        (_, True)     -> Correct
        (False, _)    -> NotSelected
        (True, False) -> Wrong

update : Action -> Model -> (Model, Effects Action)
update action model =
    case (action, model) of

        (ShowQuestion question, _) ->
            (Asking question, Effects.none)

        (ShowAnswer answer, _) ->
            (Answered answer, Effects.none)

        (Reply user, Asking question) ->
            (Answered (Answer question user), Effects.none)

        otherwise ->
            (model, Effects.none)