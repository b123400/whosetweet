module Guesser (Action, Model, init, update, view) where

import Http exposing (get)
import Json.Decode exposing (list)
import Task exposing (Task, succeed, andThen, onError)
import Html exposing (Html, div, button, text, a)
import Effects exposing (Effects, Never)
import List
import Dict exposing (Dict)
import TwitterTypes exposing (Tweet, User, Answer, Question)
import Asker exposing (Action, Model, init, update, view)
import Maybe exposing (Maybe(..))
import Random exposing (Seed)
import Random.Array
import Array exposing (Array, length)

type ViewState = Loading
               | Playing
               | Errored String -- reason
               | Finished Int Int  -- score, 10/20 correct

type alias Model =
    { viewState  : ViewState
    , tweets     : Array Tweet
    , users      : Dict String User  -- screenName : User
    , answers    : Array Answer
    , seed       : Seed
    , askerModel : Maybe Asker.Model
    }

type Action = ShowError String
            | Load
            | LoadMoreUsers
            | AddTweets (Array Tweet)
            | TryToPlay
            | Reply Question User   -- because Answer is a type, argument is the selected choice
            | Finish
            | AskerAction Asker.Action

init : (Model, Effects Action)
init = (
    { viewState = Loading
    , tweets = Array.empty
    , users = Dict.empty
    , answers = Array.empty
    , seed = Random.initialSeed 0 -- fix this with a better seed?
    , askerModel = Nothing
    }
    , loadTweets)

view : Signal.Address Action -> Model -> Html
view address model =
    case model.viewState of
        Loading ->
            div [] [ text "loading" ]

        Playing ->
            case model.askerModel of
                Just askerModel ->
                    div [] [ text ("playing, tweet count: "++(toString (length model.tweets)))
                           , Asker.view (Signal.forwardTo address AskerAction) askerModel
                           ]
                Nothing ->
                    div [] [ text ("error: no tweets found") ]

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
            ({model | viewState <- Loading }, loadTweets)

        AddTweets newTweets ->
            ((processTweets model newTweets), Effects.task (succeed TryToPlay))

        TryToPlay ->
            if model.viewState == Playing
            then (model, Effects.none)
            else
            case model.askerModel of

                -- Model already ready, now can play
                Just askerModel ->
                    ({model | viewState <- Playing}, Effects.none)

                -- Model not ready yet, make it ready
                Nothing ->
                    let (maybeAskerModel, newModel, effect) = initAskerModel model
                    in ({ newModel | askerModel <- maybeAskerModel
                                , viewState <- Playing }
                       , Effects.map AskerAction effect)

        AskerAction subAction ->
            case model.askerModel of

                Nothing ->
                    (model, Effects.none)

                Just askerModel ->
                    let (newAskerModel, askerEffect) = Asker.update subAction askerModel
                    in ({model | askerModel <- Just newAskerModel}, Effects.map AskerAction askerEffect)

        otherwise ->
            (model, Effects.none)

processTweets : Model -> Array Tweet -> Model
processTweets model tweets =
    { model | users <- Dict.union (usersDict tweets) model.users
            , tweets <- Array.append model.tweets tweets }

usersDict : Array Tweet -> Dict String User
usersDict tweets =
    tweets
    |> Array.map (\t-> (t.user.screenName, t.user))
    |> Array.toList
    |> Dict.fromList

randomQuestion : Model -> (Maybe Question, Model)
randomQuestion model =
    let (maybeTweet, seed1, newTweets) = Random.Array.choose model.seed model.tweets
    in case maybeTweet of

        Nothing ->
            (Nothing, model)

        Just tweet ->
            let (otherUsers, seed2) = model.users
                                    |> Dict.remove tweet.user.screenName
                                    |> randomUsers seed1 3
                (users, seed3) = otherUsers
                               |> Array.push tweet.user
                               |> Random.Array.shuffle seed2
            in ( Just (Question tweet users)
               , { model | tweets <- newTweets
                         , seed <- seed3 }
               )

randomUsers : Seed -> Int -> Dict String User -> (Array User, Seed)
randomUsers seed count users =
    users
    |> Dict.values
    |> Array.fromList
    |> Random.Array.shuffle seed
    |> (\(array, seed)->
        (Array.slice 0 count array, seed))


initAskerModel : Model -> (Maybe Asker.Model, Model, Effects Asker.Action)
initAskerModel model =
    let (maybeQuestion, newModel) = randomQuestion model
    in case maybeQuestion of
        Just question ->
            let (askerModel, askerAction) = Asker.init question
            in (Just askerModel, newModel, askerAction)
        Nothing ->
            (Nothing, newModel, Effects.none)

loadTweets : Effects Action
loadTweets = get (list TwitterTypes.tweet) "/recentTweets"
  `andThen`
  (\tweets ->
    succeed (AddTweets (Array.fromList tweets))
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
