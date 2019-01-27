{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}

module Main where

import           Data.Semigroup         ((<>))
import           Options.Applicative
import           Debug.Trace
import qualified Data.ByteString.Char8  as BC
import qualified Data.Time.Clock.System as Time
import qualified Data.Time.Clock        as Clock
import qualified Awesome


version :: String
version = "1.0.0"


data Options = Options
  { source   :: String
  , output   :: String
  , token    :: BC.ByteString
  , detailed :: Bool
  , verbose  :: Bool
  , noFail   :: Bool
  , html     :: Bool
  } | Version


options :: Parser Options
options =
  flag' Version ( long "version" <> short 'v' <> help "Show the version number")
  <|>
  (Options
    <$> argument str (metavar "SOURCE" <> help "JSON configuration file")
    <*> argument str (metavar "OUTPUT" <> help "Ouput destination file")
    <*> strOption (long "token" <> short 't' <> value "" <> help "Github personal access token")
    <*> switch ( long "detailed" <> short 'd' <> help "detailed awesome list")
    <*> switch ( long "verbose" <> help "show the full error description")
    <*> switch ( long "no-fail" <> help "stop at the first error")
    <*> switch ( long "html" <> help "HTML output"))


main :: IO ()
main = 
  start =<< execParser opts
  where
    opts = info
      (options <**> helper)
      (fullDesc <> header"awesome - an awesome list generator")


start :: Options -> IO ()
start Version = putStrLn version
start Options {..} = do
  a <- Time.getSystemTime
  Awesome.awesome token source output 
  b <- Time.getSystemTime
  putStrLn $ "\nFinished in " ++ show (Clock.diffUTCTime (Time.systemToUTCTime b) (Time.systemToUTCTime a))
