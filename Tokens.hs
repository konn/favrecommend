{-# LANGUAGE OverloadedStrings #-}
module Tokens (konn, twitter) where
import Web.Authenticate.OAuth

twitter :: OAuth
twitter = newOAuth { oauthServerName = "twitter"
                   , oauthRequestUri = "http://twitter.com/oauth/request_token"
                   , oauthAccessTokenUri = "http://twitter.com/oauth/access_token"
                   , oauthAuthorizeUri = "http://twitter.com/oauth/authorize"
                   , oauthSignatureMethod = HMACSHA1
                   , oauthConsumerKey = "50nHDdO7ihACAv3KbGQeQ"
                   , oauthConsumerSecret = "zPZqWx8vGF4X1amjfntuQ9Mj1d7VO7R27HnXbBUlZqg"
                   , oauthCallback = Nothing
                   , oauthRealm = Nothing
                   }

konn :: Credential
konn = Credential [("oauth_token","5965172-MTe1xb1Ssrj6iKkDjRKeUTtuv3M5tiaYfJqUoI7Ubb")
                  ,("oauth_token_secret","nMs67CKSsRtn1y31G1d5vPQdX7XixzLpDevJ2T3H8k")
                  ,("user_id","5965172")
                  ,("screen_name","mr_konn")
                  ]

