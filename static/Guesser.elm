module Guesser (Action, Model, init, update, view) where

import Http exposing (get, uriEncode)
import List
import Dict exposing (Dict)
import Maybe exposing (Maybe(..))
import Random exposing (Seed)
import Json.Decode exposing (list)
import Task exposing (Task, succeed, andThen, onError)
import Html exposing (Html, div, button, text, a, span, img, p)
import Html.Attributes exposing (class, src, href)
import Effects exposing (Effects, Never)
import TwitterTypes exposing (Tweet, User, Answer, Question)
import Asker exposing (Action, Model, init, update, view)
import Random.Array
import Array exposing (Array, length)

type ViewState = Loading
               | Playing
               | Errored String -- reason
               | Finished

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
            | AddTweets (Array Tweet)
            | TryToPlay
            | Next
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
    div [class "guesser"] [
    case model.viewState of
        Loading ->
            div [class "loading"] [ text "等陣..." ]

        Playing ->
            case model.askerModel of
                Just askerModel ->
                    div [] [ div [class "state"] [ span [class ((toString <| length <| model.answers)++" current")] [text (toString (length model.answers))]
                                                 , span [] [text "/"]
                                                 , span [class "total"] [text (toString (questionCount model))]
                                                 ]
                           , Asker.view (Signal.forwardTo address AskerAction) askerModel
                           ]
                Nothing ->
                    div [] [ text ("error: no tweets found") ]

        Errored reason ->
            div [] [ text ("error: "++reason) ]

        Finished ->
            let thisScore = score model
                total = length model.answers
            in
            div [class "finished"] [ scoreImage        model.seed thisScore total
                                   , p [class "score-text"] [text ((toString thisScore) ++ "/" ++ (toString total))]
                                   , scoreTextElement  model.seed thisScore total
                                   , tweetButton       thisScore total
                                   ]
    ]

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
                    ({model | viewState <- Playing}, Effects.task (succeed Next))

        Next ->
            if (length model.answers) == (questionCount model)
            then ({model | viewState <- Finished }, Effects.none)
            else
                let (maybeAskerModel, newModel, effect) = initAskerModel model
                in ({ newModel | askerModel <- maybeAskerModel }
                   , Effects.map AskerAction effect)

        AskerAction subAction ->
            case model.askerModel of

                Nothing ->
                    (model, Effects.none)

                Just askerModel ->
                    let (newAskerModel, askerEffect) = Asker.update subAction askerModel
                        askerResult = Asker.getResult newAskerModel
                        newAnswers = (case askerResult of
                                        Just answer -> Array.push answer model.answers
                                        Nothing     -> model.answers)
                    in ({model | askerModel <- Just newAskerModel
                               , answers <- newAnswers },
                        Effects.batch [ Effects.map AskerAction askerEffect
                                      , (case askerResult of
                                            Nothing -> Effects.none
                                            Just _ -> Effects.task (Task.sleep 1000 `andThen` \_-> (succeed Next)))])

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
    let (maybeTweet, seed1, newTweets) = choose model.seed model.tweets
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
    |> randomUsers' seed count

randomUsers' : Seed -> Int -> Array User -> (Array User, Seed)
randomUsers' seed count users =
    if count == 0
    then (Array.empty, seed)
    else
        let (maybeUser, seed1, usersLeft) = choose seed users
        in case maybeUser of
            Just user ->
                let (remaining, seed2) = randomUsers' seed1 (count-1) usersLeft
                in (Array.push user remaining, seed2)
            Nothing ->
                (Array.empty, seed1)

choose : Seed -> Array.Array a -> (Maybe a, Seed, Array.Array a)
choose seed arr =
    if arr == Array.empty
    then (Nothing, seed, arr)
    else
        let intGen = Random.int 0 (Array.length arr - 1)
            (index, seed') = Random.generate intGen seed
            front = Array.slice 0 index arr
            back = (if index+1 < Array.length arr
                    then Array.slice (index+1) (Array.length arr) arr
                    else Array.empty)
        in (Array.get index arr, seed', Array.append front back)

initAskerModel : Model -> (Maybe Asker.Model, Model, Effects Asker.Action)
initAskerModel model =
    let (maybeQuestion, newModel) = randomQuestion model
    in case maybeQuestion of
        Just question ->
            let (askerModel, askerAction) = Asker.init question
            in (Just askerModel, newModel, askerAction)
        Nothing ->
            (Nothing, newModel, Effects.none)

questionCount : Model -> Int
questionCount model =
    let totalLength = (length model.tweets)
                    + (length model.answers)
                    + (case model.askerModel of
                        Nothing -> 0
                        Just _  -> 1)
    in min totalLength 20

score : Model -> Int
score model =
    model.answers
    |> Array.filter TwitterTypes.answerCorrect
    |> Array.length

scoreImage : Seed -> Int -> Int -> Html
scoreImage seed marks total =
    let percentage = (toFloat marks) / (toFloat total) in
    if | percentage < 0.4 -> img [src "/score-bad.png"] []
       | percentage < 0.8 -> img [src "/score-medium.png"] []
       | otherwise        -> img [src "/score-good.png"] []

scoreTextElement : Seed -> Int -> Int -> Html
scoreTextElement seed marks total =
    p [class "score-text"] [ text (
    let percentage = (toFloat marks) / (toFloat total) in
    if | percentage < 0.4 ->
        let (maybeString, _) =  Random.Array.sample seed
                                  <| Array.fromList ["你完全不認得推友耶"
                                                    ,"你是來亂的嗎？"
                                                    ,"你有沒有認真做啊？"
                                                    ,"認真點做吧"]
        in Maybe.withDefault "" maybeString
       | percentage < 0.6 ->
        let (maybeString, _) =  Random.Array.sample seed
                                  <| Array.fromList ["還好還好"
                                                    ,"不錯耶"]
        in Maybe.withDefault "" maybeString

       | percentage < 0.9 ->
        let (maybeString, _) =  Random.Array.sample seed
                                  <| Array.fromList ["好棒棒"
                                                    ,"不錯耶"]
        in Maybe.withDefault "" maybeString

       | otherwise ->
        let (maybeString, _) =  Random.Array.sample seed
                                  <| Array.fromList ["超準啊！"
                                                    ,"好神！"
                                                    ,"好強！"
                                                    ,"你十分認識推友啊！"
                                                    ,"厲害"]
        in Maybe.withDefault "" maybeString
    )]

tweetButton : Int -> Int -> Html
tweetButton marks total =
    a [class "tweet-button"
      ,href <| tweetButtonSrc marks total
      ,Html.Attributes.target "_blank"
      ]
      [ text (
        let percentage = (toFloat marks) / (toFloat total) in
        if | percentage < 0.8 -> "分享結果"
           | otherwise        -> "炫耀一下"
        )
    ]

tweetButtonSrc : Int -> Int -> String
tweetButtonSrc marks total =
    "https://twitter.com/intent/tweet?text="
    ++ ((toString total)++"位推友裏面我猜中了"++(toString marks)++"個，")
    ++ (uriEncode ((
        let percentage = (toFloat marks) / (toFloat total) in
        if | percentage < 0.2 -> "我完全不了解推友們啊"
           | percentage < 0.4 -> "亂猜也中了幾個嘛！"
           | percentage < 0.6 -> "你們也來試試看猜一下推友喔"
           | percentage < 0.8 -> "我還算是滿了解推友們的啦"
           | otherwise        -> "我對推友們簡直瞭如指掌～"
    )++" http://whosetweet.b123400.net #猜推友"))

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
