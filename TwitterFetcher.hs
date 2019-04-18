{-# LANGUAGE OverloadedStrings #-}

module TwitterFetcher where

import Web.Authenticate.OAuth (OAuth, oauthConsumerKey, oauthConsumerSecret, Credential(..), def)
import Web.Twitter.Conduit (homeTimeline, twitterOAuth, TWInfo(..), TWToken(..), call)
import Web.Twitter.Types.Lens (Status, User)
import Web.Twitter.Conduit.Parameters (UserParam(..))
import Web.Twitter.Conduit.Api (friendsList)
import Web.Twitter.Conduit.Cursor (WithCursor, UsersCursorKey)

import Control.Monad.IO.Class (MonadIO, liftIO)
import Network.HTTP.Conduit (Manager)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)
import Data.Function ((&))
import Control.Lens.Setter ((?~))
import Web.Twitter.Conduit.Parameters (count)

tokens :: ByteString -> ByteString -> OAuth
tokens key secret = twitterOAuth
    { oauthConsumerKey = key
    , oauthConsumerSecret = secret
    }

credential :: T.Text -> T.Text -> Credential
credential token secret = Credential
    [ ("oauth_token", encodeUtf8 token)
    , ("oauth_token_secret", encodeUtf8 secret)
    ]

twInfo :: ByteString -> ByteString -> T.Text -> T.Text -> TWInfo
twInfo appKey appSecret token secret = def
    { twToken = def { twOAuth = (tokens appKey appSecret), twCredential = (credential token secret) }
    , twProxy = Nothing
    }

loadTweets :: MonadResource m
           => MonadIO m
           => Manager      -- network manager
           -> ByteString   -- app key
           -> ByteString   -- app secret
           -> T.Text       -- access token
           -> T.Text       -- access secret
           -> m [Status]
loadTweets manager appKey appSecret token secret =
    liftIO $ call (twInfo appKey appSecret token secret) manager (homeTimeline & count ?~ 200)

loadFollowings :: MonadResource m
               => MonadIO m
               => Manager    -- network manager
               -> ByteString -- app key
               -> ByteString -- app secret
               -> T.Text     -- access token
               -> T.Text     -- access secret
               -> Integer    -- user id
               -> m (WithCursor UsersCursorKey User)
loadFollowings manager appKey appSecret token secret userId =
    liftIO $ call (twInfo appKey appSecret token secret) manager $ friendsList (UserIdParam userId)
