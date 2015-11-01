import StartApp
import Html exposing (Html, div, button, text, a)
import Html.Events exposing (onClick)
import Html.Attributes exposing (href, class)
import Http exposing (get)
import Effects exposing (Effects, Never, task)
import Task exposing (Task, succeed, andThen, onError)
import Json.Decode exposing (dict, string)
import Debug
import Dict
import Guesser

main = app.html

app =
  StartApp.start
    { init = init
    , view = view
    , update = update
    , inputs = []
    }

port tasks : Signal (Task.Task Never ())
port tasks =
  app.tasks

view : Signal.Address Action -> Model -> Html
view address model =
  case model of
      Loading ->
        div [class "index"] [text "Loading"]

      NotLoggedIn url ->
        div [class "index"] [
          a [href url
            ,class "button"] [ text "Not logged in, click to login" ]
        ]

      LoggedIn childModel url ->
        div [class "index"] [
          a [href url
            ,class "corner"] [ text "Switch account" ]
        , Guesser.view (Signal.forwardTo address UpdateGuesser) childModel
        ]

      Errored reason ->
        div [] [text ("Error: "++reason)]

type Model = Loading
           | NotLoggedIn String               -- Login url
           | LoggedIn    Guesser.Model String -- Logout url
           | Errored     String               -- reason

type Action = Load
            | UpdateGuesser Guesser.Action
            | Show Bool String  -- logged in, url
            | ShowError String

init : (Model, Effects Action)
init = (Loading, getLoginState)

update : Action -> Model -> (Model, Effects Action)
update action model =
  case action of
    Load ->
      (model, getLoginState)

    Show True url->
      case model of
        LoggedIn childModel _ ->
          (LoggedIn childModel url, Effects.none)

        otherwise->
          let (model, effect) = Guesser.init
          in (LoggedIn model url, (Effects.map UpdateGuesser) effect)

    UpdateGuesser childAction ->

      case model of
        LoggedIn childModel url ->
          let (newModel, effect) = Guesser.update childAction childModel
          in (LoggedIn newModel url, (Effects.map UpdateGuesser) effect)

        otherwise ->
          (model, Effects.none)

    Show False url->
      (NotLoggedIn url, Effects.none)

    ShowError reason->
      (Errored reason, Effects.none)


getLoginState : Effects Action
getLoginState =
  get (dict string) "/loginState"
  `andThen`
  (\result ->
    succeed
    ( case (Dict.get "loggedIn" result,
            Dict.get "login"    result,
            Dict.get "logout"   result)
      of
        (Just "yes", _, Just logout) -> 
          Show True ("/auth"++logout)

        (Just "no",  Just login, _) -> 
          Show False ("/auth"++login)

        otherwise -> 
          ShowError "Unexpected result"
    )
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
