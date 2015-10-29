{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Data.Aeson
import Data.Text as T hiding (map)
import Data.ByteString (ByteString)
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth
import Network.HTTP.Client.Conduit (Manager, newManager)
import TwitterFetcher
import Debug.Trace (trace)

data MyApp = MyApp
    { httpManager :: Manager
    }

instance Yesod MyApp where
    approot = ApprootStatic "http://localhost:3000"

mkYesod "MyApp" [parseRoutes|
/             HomeR  GET
/auth         AuthR  Auth getAuth
/recentTweets TweetR GET
|]

instance RenderMessage MyApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth MyApp where
    type AuthId MyApp = T.Text

    loginDest _ = HomeR
    logoutDest _ = HomeR
    authPlugins _ = [authTwitter clientKey clientSecret]
    maybeAuthId = lookupSession "_ID"
    authHttpManager = httpManager

    authenticate creds =
        let
            accessKey    = findAccessToken creds
            accessSecret = findAccessSecret creds
            screenName   = findScreenName creds
        in
        case (accessKey, accessSecret, screenName) of
            (Just key, Just secret, Just name) ->
                setSession "accessKey" key >>
                setSession "accessSecret" secret >>
                (return $ Authenticated $ name)
            otherwise ->
                return $ ServerError "Missing params"

firstKey :: [(a, b)] -> (a -> Bool) -> Maybe b
firstKey []          _ = Nothing
firstKey ((a,b):xs) fn
    | fn a             = Just b
    | otherwise        = firstKey xs fn

findAccessToken :: Creds MyApp -> Maybe Text
findAccessToken creds = firstKey (credsExtra creds) (=="oauth_token")

findAccessSecret :: Creds MyApp -> Maybe Text
findAccessSecret creds = firstKey (credsExtra creds) (=="oauth_token_secret")

findScreenName :: Creds MyApp -> Maybe Text
findScreenName creds = firstKey (credsExtra creds) (=="screen_name")

getSessionAccessToken :: MonadHandler m => m (Maybe Text)
getSessionAccessToken = lookupSession "accessKey"

getSessionAccessSecret :: MonadHandler m => m (Maybe Text)
getSessionAccessSecret = lookupSession "accessSecret"

getHomeR :: Handler Html
getHomeR = do
    maid <- maybeAuthId
    defaultLayout
        [whamlet|
            <p>Your current auth ID: #{show maid}
            $maybe _ <- maid
                <p>
                    <a href=@{AuthR LogoutR}>Logout
            $nothing
                <p>
                    <a href=@{AuthR LoginR}>Go to the login page
        |]

data Person = Person
    { name :: Text
    , age  :: Int
    }

instance ToJSON Person where
    toJSON Person {..} = object
        [ "name" .= name
        , "age"  .= age
        ]

fuck :: MyApp -> String
fuck app = "Fuck"

getTweetR :: Handler Value
getTweetR = do
    app <- getYesod
    token <- getSessionAccessToken
    secret <- getSessionAccessSecret
    case (token, secret) of
        (Just t, Just s) -> do
            receivedTweets <- (loadTweets (authHttpManager app) t s)
            trace (show receivedTweets) (return $ toJSON $ Person "Michael" 28)
        otherwise ->
            (return $ toJSON $ Person "Nothing" 0)

main :: IO ()
main = newManager >>= \manager ->
    warp 3000 $ MyApp manager
