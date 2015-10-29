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
import Network.HTTP.Types (Status, status400, status200)
import Network.Wai (responseLBS, Response)
import Web.Twitter.Conduit.Cursor as Cursor
import Yesod.Static
import Yesod.Core.Handler
import Blaze.ByteString.Builder (toByteString)
import Data.Text.Encoding (decodeUtf8)

staticFiles "static"

data MyApp = MyApp
    { httpManager :: Manager
    , getStatic :: Static
    }

instance Yesod MyApp where
    approot = ApprootStatic "http://localhost:3000"

mkYesod "MyApp" [parseRoutes|
/auth         AuthR       Auth getAuth
/recentTweets TweetR      GET
/followings   FollowingR  GET
/loginState   LoginStateR GET
!/            HomeR       GET
!/            StaticR     Static getStatic
|]

instance RenderMessage MyApp FormMessage where
    renderMessage _ _ = defaultFormMessage

instance YesodAuth MyApp where
    type AuthId MyApp = T.Text

    loginDest _ = StaticR (StaticRoute [] [])
    logoutDest _ = StaticR (StaticRoute [] [])
    authPlugins _ = [authTwitter clientKey clientSecret]
    maybeAuthId = lookupSession "_ID"
    authHttpManager = httpManager

    authenticate creds =
        let
            accessKey    = findAccessToken creds
            accessSecret = findAccessSecret creds
            screenName   = findScreenName creds
            userId       = findUserId creds
        in
        case ( accessKey
             , accessSecret
             , screenName
             , userId
             )
        of
            (Just key, Just secret, Just name, Just userId) ->
                setSession "accessKey" key >>
                setSession "accessSecret" secret >>
                setSession "userId" userId >>
                (return $ Authenticated $ name)
            otherwise ->
                return $ ServerError "Missing params"

firstKey :: Show a => Show b => [(a, b)] -> (a -> Bool) -> Maybe b
firstKey []          _ = Nothing
firstKey ((a,b):xs) fn
    | fn a             = Just b
    | otherwise        = trace (show xs) (firstKey xs fn)

findAccessToken :: Creds MyApp -> Maybe Text
findAccessToken creds = firstKey (credsExtra creds) (=="oauth_token")

findAccessSecret :: Creds MyApp -> Maybe Text
findAccessSecret creds = firstKey (credsExtra creds) (=="oauth_token_secret")

findScreenName :: Creds MyApp -> Maybe Text
findScreenName creds = firstKey (credsExtra creds) (=="screen_name")

findUserId :: Creds MyApp -> Maybe Text
findUserId creds = firstKey (credsExtra creds) (=="user_id")

getSessionAccessToken :: MonadHandler m => m (Maybe Text)
getSessionAccessToken = lookupSession "accessKey"

getSessionAccessSecret :: MonadHandler m => m (Maybe Text)
getSessionAccessSecret = lookupSession "accessSecret"

getSessionUserId :: MonadHandler m => m (Maybe Text)
getSessionUserId = lookupSession "userId"

replyJson :: Status -> Text -> Response
replyJson status text = responseLBS
    status
    [("Content-Type", "application/json")]
    $ encode $ object [ ("message" .= text) ]

getLoginStateR :: Handler Value
getLoginStateR = 
    getYesod >>= \app->
    maybeAuthId >>= \maid-> 
    sendWaiResponse
    $ responseLBS status200 [("Content-Type", "application/json")]
    $ encode $ object [
     ("key"   .= ("value" :: Text)),
     ("login" .= (generateUrl app LoginR)),
     ("logout" .= (generateUrl app LogoutR)),
     ("state" .= ((case maid of
        Just userID -> "yes"
        Nothing     -> "no") :: Text))
    ]

generateUrl :: RenderRoute a => Yesod site => site -> Route a -> Text
generateUrl app route = let (paths, params) = renderRoute route
                        in decodeUtf8 $ toByteString $ (joinPath app "" paths params)

getTweetR :: Handler Value
getTweetR = getYesod >>= \app ->
            getSessionAccessToken >>= \token ->
            getSessionAccessSecret >>= \secret ->
            case (token, secret) of
                (Just t, Just s) ->
                    (loadTweets (authHttpManager app) t s) >>=
                    return . toJSON
                otherwise ->
                    sendWaiResponse $ replyJson status400 "Access token not found"

getFollowingR :: Handler Value
getFollowingR = getYesod >>= \app ->
                getSessionAccessToken >>= \token ->
                getSessionAccessSecret >>= \secret ->
                getSessionUserId >>= \userId ->
                case (token, secret, userId) of
                    (Just t, Just s, Just u) ->
                        (loadFollowings (authHttpManager app) t s (read $ T.unpack u)) >>=
                        return . toJSON . Cursor.contents
                    otherwise ->
                        sendWaiResponse $ replyJson status400 "Access token not found"

getHomeR :: Handler Html
getHomeR = getYesod >>= \app->
           let indexPath = "static/index.html"
           in sendFile "text/html" indexPath

main :: IO ()
main = do
    manager <- newManager
    static@(Static settings) <- static "static"
    warp 3000 $ MyApp manager static
