{-# LANGUAGE OverloadedStrings #-}
module Tokens (konn, twitter) where
import Web.Authenticate.OAuth

twitter :: OAuth
twitter = OAuth { oauthServerName = "twitter"
                , oauthRequestUri = "http://twitter.com/oauth/request_token"
                , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
                , oauthAuthorizeUri = "http://twitter.com/oauth/authorize"
                , oauthSignatureMethod = HMACSHA1
                , oauthConsumerKey = "XXX"
                , oauthConsumerSecret = "XXX"
                , oauthCallback = Nothing
                }

konn :: Credential
konn = Credential [("oauth_token","XXX")
                  ,("oauth_token_secret","XXX")
                  ]

