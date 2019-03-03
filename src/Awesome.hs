{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RecordWildCards       #-}

module Awesome (awesome) where

import qualified Awesome.Config           as Config
import qualified Awesome.Github           as Github
import qualified Awesome.Markdown         as MD
import qualified Control.Concurrent.Async as Async
import           Control.Monad
import qualified Data.ByteString          as B
import qualified Data.ByteString.Char8    as BC
import           Data.Char                (toLower)
import           Data.Functor
import           Data.Int                 (Int64)
import           Data.List                (sortOn)
import           Data.List.Split          (splitOn)
import           Data.Maybe               (catMaybes, fromMaybe)
import           Data.Ord                 (Down (..))
import           Data.Text.Format         (Only (..), format)
import           Data.Text.Lazy           (Text)
import qualified Data.Text.Lazy           as T
import           Data.Text.Lazy.IO        (hPutStr, putStrLn)
import qualified Data.Time.LocalTime      as Time
import           Data.Traversable
import           Prelude                  hiding (hPutStr, putStrLn)
import           System.IO                (Handle, IOMode (..), hClose, hFlush,
                                           openFile)
import qualified System.Directory as Dir
import           System.FilePath.Posix    ((</>), takeDirectory)
import qualified Paths_awesome


-- Entry point to convert the JSON file to Markdown.
awesome :: Github.Token -> FilePath -> FilePath -> IO ()
awesome token src dst =
  Config.read src (writeCategoryToFile token dst) 
    >> Dir.makeAbsolute dst >>= (\s -> copyAssets (takeDirectory s))

-- Copy asset images in the output directory.
copyAssets :: FilePath -> IO ()
copyAssets outputDir = do
  Dir.createDirectoryIfMissing False destDir
  Paths_awesome.getDataFileName "assets/star.png" >>= flip Dir.copyFile (destDir </> "star.png")
  Paths_awesome.getDataFileName "assets/license.png" >>= flip Dir.copyFile (destDir </> "license.png") 
  where
    destDir = outputDir </> "assets"


-- Get the owner/repo info from the url.
githubInfo :: Text -> Maybe (Github.RepoOwner, Github.RepoName)
githubInfo u =
  if length s < 2 then Nothing
  else Just ((last . init) s, last s)
  where s = T.splitOn "/" u


-- Fetch the information about Github repository if the link is a Github link.
githubRequest :: Github.Token -> Text -> IO (Maybe MD.Link)
githubRequest token url =
  join <$> for (githubInfo url) (\(o, r) ->
    Github.req token o r <&> fmap repoToLink)


-- Convert the Github repository data to mardown link.
repoToLink :: Github.Repo -> MD.Link
repoToLink Github.Repo {..} =
  let
    licName = maybe "" Github.spdx_id license
    licUrl  = maybe "" (fromMaybe "" . Github.url) license
  in
    MD.GithubLink name html_url stargazers_count licName licUrl description


-- Convert a Item to Link (Github or External). 
itemToLink :: Github.Token -> Config.Item -> IO (Maybe MD.Link)
itemToLink token (Config.Item u n d) =
  if Config.isGithub u then
    githubRequest token u
  else
    return $ n >>= (\title -> Just $ MD.ExternLink title u (fromMaybe "" d))


-- Generate the links.
itemsToLinks :: Github.Token -> [Config.Item] -> IO [MD.Link]
itemsToLinks token =
  fmap (sortOn Down . catMaybes) . traverse (itemToLink token)


-- Generate the links concurrently.
itemsToLinksAsync :: Github.Token -> [Config.Item] -> IO [Maybe MD.Link]
itemsToLinksAsync token =
  Async.mapConcurrently (itemToLink token)


-- Similar to itemsToLinksAsync but sort and remove the invalid links.
itemsToLinksAsync' :: Github.Token -> [Config.Item] -> IO [MD.Link]
itemsToLinksAsync' token =
  fmap (sortOn Down . catMaybes) . itemsToLinksAsync token


-- Write a category in the file. The category must be the root category.
writeCategoryToFile :: Github.Token -> FilePath -> Config.Category -> IO ()
writeCategoryToFile token file cat = do
  h <- openFile file WriteMode
  putStrLn "awesome list generation"
  putStrLn "======================="
  hPutStr h $ showCategoryHeader cat
  hPutStr h "\n\n"
  forM_ (Config.categories cat) $ writeTOC h 0
  forM_ (Config.categories cat) $ writeCategories token h 2
  writeFooter h
  hClose h


-- Write the Table Of Content in the handle.
writeTOC :: Handle -> Int64 -> [Config.Category] -> IO()
writeTOC h level = mapM_ (\c ->
    let
      s = T.replicate (level*2) " "
      t = Config.title c
      l = T.toLower $ T.replace " " "-" t
      res = format "{}- [{}](#{})\n" (s, t, l)
    in
      hPutStr h res >> mapM_ (writeTOC h (level+1)) (Config.categories c))


-- Write all categories in the handle.
writeCategories :: Github.Token -> Handle -> Int64 -> [Config.Category] -> IO ()
writeCategories token h l =
  mapM_ (writeCategory token h l)


-- Write a category in the handle.
writeCategory :: Github.Token -> Handle -> Int64 -> Config.Category -> IO ()
writeCategory token h l c = do
  hPutStr h $ showCategory l c
  forM_ (Config.items c) $ writeLinks h . itemsToLinks token
  forM_ (Config.categories c) $ writeCategories token h (l+1)


-- Write the list of links in the handle.
writeLinks :: Handle -> IO [MD.Link] -> IO ()
writeLinks h =
  (=<<) $ mapM_ (hPutStr h . MD.show)


-- Write the footer with the generation date.
writeFooter :: Handle -> IO ()
writeFooter h = do
  t <- Time.getZonedTime
  hPutStr h $ format "\n\n\n_List generated with [awesome](https//github.com/gribouille/awesome) at {}._\n" (Only t)


-- Show the category in function of title level.
showCategory :: Int64 -> Config.Category -> Text
showCategory level (Config.Category t d _ _) =
  format "\n\n{} {}\n\n{}" (T.replicate level "#", t, dd)
    where
      dd = maybe "" (\x -> if x == "" then "" else format "*{}*\n\n" (Only x)) d


-- Show the title and the description of the top category.
showCategoryHeader :: Config.Category -> Text
showCategoryHeader cat =
  format "# {}\n\n{}" (t, d)
    where
      t = Config.title (cat :: Config.Category)
      d = fromMaybe "" $ Config.description (cat :: Config.Category)


