import Html exposing (div, button, text, a)
import Html.Events exposing (onClick)
import StartApp
import Http exposing (get)
import Html.Attributes exposing (href)
import Effects exposing (Effects, Never)
import Task exposing (Task, succeed, andThen, onError)
import Json.Decode exposing (dict, string)
import Debug
import Dict

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

view address model =
  case model of
      Loading ->
        div [] [text "Loading"]
        

      NotLoggedIn url ->
        div [] [
          a [href url] [ text "Not logged in, click to login" ]
        ]

      LoggedIn url ->
        div [] [
          a [href url] [ text "Logged in: click to logout" ]
        ]

      Errored reason ->
        div [] [text ("Error: "++reason)]

type Model = Loading
           | NotLoggedIn String -- Login url
           | LoggedIn    String -- Logout url
           | Errored     String -- reason

type Action = Load
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
      (LoggedIn url, Effects.none)

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
