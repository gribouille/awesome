{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DataKinds             #-}


module Awesome.Github where

import           GHC.Generics           (Generic)
import           Data.Default           (def)
import qualified Data.Text              as T
import qualified Data.ByteString.Char8  as BC
import qualified Data.Aeson             as A
import qualified Network.HTTP.Req       as R 
import           Network.HTTP.Req       ((/:))
import qualified Network.HTTP.Client    as HC
import           Text.Printf            (printf)
import qualified Control.Exception      as E
import qualified System.Console.Pretty  as P


type Token      = BC.ByteString
type RepoOwner  = T.Text
type RepoName   = T.Text


-- Github API repository JSON object.
data Repo = Repo
  { name             :: String
  , description      :: String
  , created_at       :: String
  , updated_at       :: String
  , pushed_at        :: String
  , homepage         :: Maybe String
  , html_url         :: String
  , stargazers_count :: Int
  , watchers_count   :: Int
  , license          :: Maybe License
  } deriving (Show, Generic, A.FromJSON)


-- Github API license JSON object.
data License = License
  { spdx_id :: String
  , url     :: Maybe String
  } deriving (Show, Generic, A.FromJSON)


-- Github API error body JSON object.
newtype Error = Error
  { message :: String }
  deriving (Show, Generic, A.FromJSON)


-- Build the url to REST Github API v3.
buildURL :: RepoOwner -> RepoName -> R.Url 'R.Https
buildURL owner repo =
  R.https "api.github.com" /: "repos" /: owner /: repo


-- Execute a request to the Github API to get info about the {owner}/{repo}.
runReq :: Token -> RepoOwner -> RepoName -> IO Repo
runReq token user repo =
  R.runReq def $ 
    reqUnsafe token (buildURL user repo) >>= (\r -> 
      return (R.responseBody r :: Repo))


--
reqUnsafe :: Token -> R.Url 'R.Https -> R.Req (R.JsonResponse Repo)
reqUnsafe token url =
  R.req R.GET url R.NoReqBody R.jsonResponse $
       R.header "User-Agent"    "awesome"
    <> R.header "Authorization" (BC.append "token " token)


--
req :: Token -> RepoOwner -> RepoName -> IO (Maybe Repo)
req token user repo =
  E.try (runReq token user repo) >>= either
    -- (\e -> putStrLn (showExcept e) >> return Nothing)
    (\e -> let _ = (e :: R.HttpException) in statusMessage P.Red "✘" user repo >> return Nothing)
    (\r -> statusMessage P.Green "✔" user repo >> return (Just r))


statusMessage :: P.Color -> T.Text -> RepoOwner -> RepoName -> IO ()
statusMessage color sym user repo =
  let
    name =  user `mappend` "/" `mappend` repo
    space = replicate (70 - T.length name) ' '
    ok = P.color color sym 
  in
    putStrLn $ T.unpack name ++ space ++ T.unpack ok

--
showExcept :: R.HttpException -> String
showExcept (R.VanillaHttpException e) = showExcept' e
showExcept (R.JsonHttpException s) = s


--
showExcept' :: HC.HttpException -> String
showExcept' e@(HC.HttpExceptionRequest r (HC.StatusCodeException _ body)) =
  let
    p = BC.unpack (HC.path r)
    b = A.decodeStrict body :: Maybe Error
  in
    maybe (show e) (\Error { message = m} -> 
      printf "error request for %s: %s" p m) b
showExcept' e@(HC.HttpExceptionRequest r _) = show e
showExcept' (HC.InvalidUrlException url reason) = 
  printf "invalid URL %s: %s" url reason