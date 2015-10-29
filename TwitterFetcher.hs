{-# LANGUAGE OverloadedStrings #-}

module TwitterFetcher where

import Web.Authenticate.OAuth (OAuth, oauthConsumerKey, oauthConsumerSecret, Credential(..), def)
import Web.Twitter.Conduit (homeTimeline, twitterOAuth, TWInfo(..), TWToken(..), call)
import Web.Twitter.Types.Lens (Status, User)
import Web.Twitter.Conduit.Parameters (UserParam(..))
import Web.Twitter.Conduit.Api (friendsList)
import Web.Twitter.Conduit.Cursor (WithCursor, UsersCursorKey)

import Network.HTTP.Conduit (Manager)
import Control.Monad.Trans.Resource (MonadResource)
import qualified Data.Conduit.List as CL
import qualified Data.Text as T
import Data.ByteString (ByteString)
import Data.Text.Encoding (encodeUtf8)

clientKey :: ByteString
clientKey = "l2GlANAaQcWk8EcwgFKDeRIsy"

clientSecret :: ByteString
clientSecret = "WtYgEtHkmkvrD1g69OXfbcTnRPgrJ6p8yK31NcbhjoXCD0Kq7m"

tokens :: OAuth
tokens = twitterOAuth
    { oauthConsumerKey = clientKey
    , oauthConsumerSecret = clientSecret
    }

credential :: T.Text -> T.Text -> Credential
credential token secret = Credential
    [ ("oauth_token", encodeUtf8 token)
    , ("oauth_token_secret", encodeUtf8 secret)
    ]

twInfo :: T.Text -> T.Text -> TWInfo
twInfo token secret = def
    { twToken = def { twOAuth = tokens, twCredential = (credential token secret) }
    , twProxy = Nothing
    }

loadTweets :: MonadResource m
           => Manager      -- network manager
           -> T.Text       -- access token
           -> T.Text       -- access secret
           -> m [Status]
loadTweets manager token secret =
    call (twInfo token secret) manager homeTimeline

loadFollowings :: MonadResource m
               => Manager    -- network manager
               -> T.Text     -- access token
               -> T.Text     -- access secret
               -> Integer    -- user id
               -> m (WithCursor UsersCursorKey User)
loadFollowings manager token secret userId =
    call (twInfo token secret) manager $ friendsList (UserIdParam userId)
