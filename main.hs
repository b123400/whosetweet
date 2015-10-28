{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE QuasiQuotes           #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE TypeFamilies          #-}

import Data.Text as T hiding (map)
import Data.ByteString (ByteString)
import Yesod
import Yesod.Auth
import Yesod.Auth.OAuth
import Network.HTTP.Client.Conduit (Manager, newManager)

data MyApp = MyApp
    { httpManager :: Manager
    }

instance Yesod MyApp where
    approot = ApprootStatic "http://localhost:3000"

mkYesod "MyApp" [parseRoutes|
/     HomeR GET
/auth AuthR Auth getAuth
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
            accessKey    = getAccessKey creds
            accessSecret = getAccessSecret creds
            screenName   = getScreenName creds
        in
        case (accessKey, accessSecret, screenName) of
            (Just key, Just secret, Just name) ->
                setSession "accessKey" key >>
                setSession "accessSecret" secret >>
                (return $ Authenticated $ name)
            otherwise ->
                return $ ServerError "Missing params"

clientKey :: ByteString
clientKey = "l2GlANAaQcWk8EcwgFKDeRIsy"

clientSecret :: ByteString
clientSecret = "WtYgEtHkmkvrD1g69OXfbcTnRPgrJ6p8yK31NcbhjoXCD0Kq7m"

firstKey :: [(a, b)] -> (a -> Bool) -> Maybe b
firstKey []          _ = Nothing
firstKey ((a,b):xs) fn
    | fn a             = Just b
    | otherwise        = firstKey xs fn

getAccessKey :: Creds MyApp -> Maybe Text
getAccessKey creds = firstKey (credsExtra creds) (=="oauth_token")

getAccessSecret :: Creds MyApp -> Maybe Text
getAccessSecret creds = firstKey (credsExtra creds) (=="oauth_token_secret")

getScreenName :: Creds MyApp -> Maybe Text
getScreenName creds = firstKey (credsExtra creds) (=="screen_name")

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


main :: IO ()
main = newManager >>= \manager ->
    warp 3000 $ MyApp manager
