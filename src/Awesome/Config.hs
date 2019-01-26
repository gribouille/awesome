{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE DeriveAnyClass        #-}
{-# LANGUAGE DeriveGeneric         #-}

module Awesome.Config where

import           GHC.Generics           (Generic)
import           Data.Default           (def)
import qualified Data.Text              as T
import qualified Data.ByteString.Char8  as BC
import qualified Data.Aeson             as A
import qualified Data.ByteString.Lazy   as BS
import           Data.List              (isPrefixOf)


-- URL item in the generated markdown file.
data Item = Item
  { url         :: String -- required: can have the format "{owner}/{repo}" for github link
  , name        :: Maybe String -- optional for github link
  , description :: Maybe String -- optional for github link
  } deriving (Show, Generic, A.FromJSON)


-- Category in the generated markdown file.
data Category = Category
  { title       :: String
  , description :: Maybe String
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
isGithub :: String -> Bool
isGithub u = 
  ("https://github.com" `isPrefixOf` u) ||
    (not ("https://" `isPrefixOf` u) && not ("http://" `isPrefixOf` u))
  


