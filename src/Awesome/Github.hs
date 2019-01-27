{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}

module Awesome.Github where

import qualified Control.Exception     as E
import qualified Data.Aeson            as A
import qualified Data.ByteString.Char8 as BC
import           Data.Default          (def)
import           Data.Text.Format      (format)
import           Data.Text.Lazy        (Text)
import qualified Data.Text.Lazy        as T
import           Data.Text.Lazy.IO     (putStrLn)
import           GHC.Generics          (Generic)
import qualified Network.HTTP.Client   as HC
import           Network.HTTP.Req      ((/:))
import qualified Network.HTTP.Req      as R
import           Prelude               hiding (putStrLn)
import qualified System.Console.Pretty as P


-- Github developper token: https://github.com/settings/tokens
type Token      = BC.ByteString
-- Repository owner.
type RepoOwner  = Text
-- Repository name.
type RepoName   = Text


-- Github API repository JSON object.
data Repo = Repo
  { name             :: Text
  , description      :: Text
  , created_at       :: Text
  , updated_at       :: Text
  , pushed_at        :: Text
  , homepage         :: Maybe Text
  , html_url         :: Text
  , stargazers_count :: Int
  , watchers_count   :: Int
  , license          :: Maybe License
  } deriving (Show, Generic, A.FromJSON)


-- Github API license JSON object.
data License = License
  { spdx_id :: Text
  , url     :: Maybe Text
  } deriving (Show, Generic, A.FromJSON)


-- Github API error body JSON object.
newtype Error = Error
  { message :: Text }
  deriving (Show, Generic, A.FromJSON)


-- Build the url to REST Github API v3.
buildURL :: RepoOwner -> RepoName -> R.Url 'R.Https
buildURL owner repo =
  R.https "api.github.com" /: "repos" /: T.toStrict owner /: T.toStrict repo


-- Execute a Github API request to get info about the {owner}/{repo}.
runReq :: Token -> RepoOwner -> RepoName -> IO Repo
runReq token user repo =
  R.runReq def $
    reqUnsafe token (buildURL user repo) >>= (\r ->
      return (R.responseBody r :: Repo))


-- Request to the Github API (throw exception if the request failed).
reqUnsafe :: Token -> R.Url 'R.Https -> R.Req (R.JsonResponse Repo)
reqUnsafe token url =
  R.req R.GET url R.NoReqBody R.jsonResponse $
       R.header "User-Agent"    "awesome"
    <> R.header "Authorization" (BC.append "token " token)


-- Similar to reqUnsafe but catch the exception.
req :: Token -> RepoOwner -> RepoName -> IO (Maybe Repo)
req token user repo =
  E.try (runReq token user repo) >>= either
    -- (\e -> putStrLn (showExcept e) >> return Nothing)
    (\e -> let _ = (e :: R.HttpException) in statusMessage P.Red "✘" user repo >> return Nothing)
    (\r -> statusMessage P.Green "✔" user repo >> return (Just r))


-- Print the request status.
statusMessage :: P.Color -> Text -> RepoOwner -> RepoName -> IO ()
statusMessage color sym user repo =
  let
    name =  user `mappend` "/" `mappend` repo
    space = T.replicate (70 - T.length name) " "
    ok = P.color color (T.toStrict sym)
  in
    putStrLn $ name <> space <>  T.fromStrict ok

    
--
showExcept :: R.HttpException -> Text
showExcept (R.VanillaHttpException e) = showExcept' e
showExcept (R.JsonHttpException s)    = T.pack s


--
showExcept' :: HC.HttpException -> Text
showExcept' e@(HC.HttpExceptionRequest r (HC.StatusCodeException _ body)) =
  let
    p = BC.unpack (HC.path r)
    b = A.decodeStrict body :: Maybe Error
  in
    maybe (T.pack (show e)) (\Error { message = m} ->
      format "error request for {}: {}" (p, m)) b
showExcept' e@(HC.HttpExceptionRequest r _) = T.pack $ show e
showExcept' (HC.InvalidUrlException url reason) =
  format "invalid URL {}: {}" (url, reason)
