{-# LANGUAGE AllowAmbiguousTypes   #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ExplicitForAll        #-}
{-# LANGUAGE InstanceSigs          #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE RecordWildCards       #-}

module Awesome.Markdown where

import qualified Awesome.Config as Config
import           Text.Printf            (printf)


-- Mardown representation of a link item.
data Link = GithubLink
  { title      :: String
  , url        :: String
  , star       :: Int
  , license    :: String
  , urlLicense :: String
  , desc       :: String
  } | ExternLink
  { title :: String
  , url   :: String
  , desc  :: String
  } deriving (Eq)


instance Show Link where
  show :: Link -> String
  show GithubLink{..} = 
    printf "* [%s](%s) %s %d %s- %s\n" title url starSVG star l desc
      where 
        l = if license == "" || urlLicense == "" then 
            "" 
          else 
            printf "[[%s](%s)] " license urlLicense :: String
  show ExternLink{..} = 
    if desc == "" then
      printf "* [%s](%s)\n" title url
    else
      printf "* [%s](%s) - %s\n" title url desc


-- GithubLink links priority and ordered by stars.
instance Ord Link where
  compare :: Link -> Link -> Ordering
  compare ExternLink{..} _                            = LT
  compare GithubLink{star = s1} GithubLink{star = s2} = compare s1 s2
  compare _ _                                         = GT
  

starSVG :: String
starSVG = "<svg class=\"octicon octicon-star\" viewBox=\"0 0 14 16\" version=\"1.1\" width=\"14\" height=\"16\" aria-hidden=\"true\"><path fill-rule=\"evenodd\" d=\"M14 6l-4.9-.64L7 1 4.9 5.36 0 6l3.6 3.26L2.67 14 7 11.67 11.33 14l-.93-4.74L14 6z\"></path></svg>"

