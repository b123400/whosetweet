module Guesser (Action, Model, init, update, view) where

import Http exposing (get)
import Json.Decode exposing (list)
import Task exposing (Task, succeed, andThen, onError)
import Html exposing (Html, div, button, text, a)
import Effects exposing (Effects, Never)
import List exposing (length, head)
import TwitterTypes exposing (Tweet, User, Answer, Question)
import Asker exposing (Action, Model, init, update, view)
import Maybe

type ViewState = Loading
               | Playing Asker.Model
               | Errored String -- reason
               | Finished Int Int  -- score, 10/20 correct

type alias Model =
    { viewState  : ViewState
    , tweets     : (List Tweet)     -- tweets that have not seen in any question
    , questions  : List Question
    , extraUsers : List User      -- in case if we need more user to make choices
    , answers    : List Answer
    }

type Action = ShowError String
            | Load
            | LoadMoreUsers
            | AddTweets (List Tweet)
            | AddUsers (List User)
            | Play
            | Reply Question User   -- because Answer is a type, argument is the selected choice
            | Finish
            | AskerAction Asker.Action

init : (Model, Effects Action)
init = (
    { viewState = Loading
    , tweets = []
    , questions = []
    , extraUsers = []
    , answers = []
    }
    , loadTweets)

view : Signal.Address Action -> Model -> Html
view address model =
    case model.viewState of
        Loading ->
            div [] [ text "loading" ]

        Playing askerModel ->
            div [] [ text ("playing, tweet count: "++(toString (length model.tweets)))
                   , Asker.view (Signal.forwardTo address AskerAction) askerModel
                   ]

        Errored reason ->
            div [] [ text ("error: "++reason) ]

        Finished corrected total ->
            div [] [ text ("score: "++(toString corrected)++"/"++(toString total)) ]

update : Action -> Model -> (Model, Effects Action)
update action model =
    case action of
        ShowError reason ->
            ({model | viewState <- Errored reason }, Effects.none)

        Load ->
            (model, loadTweets)

        AddTweets newTWeets ->
            ({model | tweets <- model.tweets ++ newTWeets }, Effects.task (succeed Play))

        Play ->
            let maybeModel = findAskerModel model
            in case maybeModel of

                Just askerModel ->
                    ({model | viewState <- Playing askerModel}, Effects.none)

                otherwise->
                    ({model | viewState <- ShowError "No tweet found"}, Effects.none)

        AskerAction subAction ->
            let maybeModel = findAskerModel model
            in case maybeModel of

                Just askerModel ->
                    let (newAskerModel, askerEffect) = Asker.update subAction askerModel
                    in ({model | viewState <- Playing newAskerModel}, (Effects.map AskerAction) askerEffect)

                otherwise ->
                    (model, Effects.none)

        otherwise ->
            (model, Effects.none)


findAskerModel : Model -> Maybe Asker.Model
findAskerModel model =
    case model.viewState of
        Playing askerModel ->
            Just askerModel

        otherwise ->
            -- Fix this: return effect
            randomQuestion model.tweets
            `Maybe.andThen`
            (\question ->
                let (model, effect) = Asker.init question
                in Just model
            )

randomQuestion : List Tweet -> Maybe Question
randomQuestion tweets =
    head tweets
    `Maybe.andThen`
    (\tweet->
        Just (Question tweet [tweet.user])
    )

loadTweets : Effects Action
loadTweets = get (list TwitterTypes.tweet) "/recentTweets"
  `andThen`
  (\tweets ->
    succeed (AddTweets tweets)
  )
  `onError`
  (\error->
    case error of
      Http.Timeout ->
        succeed (ShowError "Timeout")

      Http.NetworkError ->
        succeed (ShowError "Network error")

      Http.UnexpectedPayload payload ->
        succeed (ShowError "wrong payload")

      Http.BadResponse code status ->
        succeed (ShowError ("bad response: "++status))
  )
  |> Effects.task
