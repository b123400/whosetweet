module TwitterTypes ( Tweet
                    , User
                    , Question
                    , Answer
                    , tweet
                    , user
                    ) where

import Json.Decode exposing (Decoder, dict, string, list, object2, (:=))

type alias Tweet =
    { user : User
    , text : String
    }

type alias User =
    { userName   : String
    , screenName : String
    }

type alias Question =
    { tweet   : Tweet
    , choices : List User
    }

type alias Answer =
    { question   : Question
    , answer     : User
    }

tweet : Decoder Tweet
tweet =
    object2 Tweet
        ("user" := user)
        ("text" := string)

user : Decoder User
user =
    object2 User
        ("name" := string)
        ("screen_name" := string)