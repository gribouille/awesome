{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}

module Awesome.Config where

import qualified Data.Aeson            as A
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy  as BS
import           Data.Default          (def)
import           Data.Text.Lazy        (Text)
import qualified Data.Text.Lazy        as T
import           GHC.Generics          (Generic)


-- URL item in the generated markdown file.
data Item = Item
  { url         :: Text       -- required: can have the format "{owner}/{repo}" for github link
  , name        :: Maybe Text -- optional for github link
  , description :: Maybe Text -- optional for github link
  } deriving (Show, Generic, A.FromJSON)


-- Category in the generated markdown file.
data Category = Category
  { title       :: Text
  , description :: Maybe Text
  , categories  :: Maybe [Category]
  , items       :: Maybe [Item]
  } deriving (Show, Generic, A.FromJSON)


-- Read the awesome JSON configuration file.
read :: FilePath -> (Category -> IO ()) -> IO()
read file fn =
  BS.readFile file >>= (\raw ->
    case (A.decode raw :: Maybe Category) of
      Nothing -> putStrLn $  "invalid file: " ++ file
      Just c  -> fn c)


-- In the configuration file, a Github repository can be specify with the full
-- url of only the "{owner}/{repo}" format.
isGithub :: Text -> Bool
isGithub u =
  ("https://github.com" `T.isPrefixOf` u) ||
    (not ("https://" `T.isPrefixOf` u) && not ("http://" `T.isPrefixOf` u))



