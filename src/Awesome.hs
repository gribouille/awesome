{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RecordWildCards       #-}

module Awesome (awesome) where 

import           Data.Functor
import           Data.Traversable
import           Control.Monad
import qualified Data.Text              as T
import qualified Data.ByteString        as B
import qualified Data.ByteString.Char8  as BC
import           Data.Char              (toLower)
import           Data.List              (sortOn)
import           Data.Ord               (Down (..))
import           Data.List.Split        (splitOn)
import           Data.Maybe             (fromMaybe, catMaybes)
import           System.IO              (Handle, IOMode (..), hClose, hFlush,
                                         hPutStr, openFile)
import           Text.Printf            (printf)
import qualified Control.Concurrent.Async as Async

import qualified Awesome.Github as Github
import qualified Awesome.Markdown as MD
import qualified Awesome.Config as Config


-- Entry point to convert the JSON file to Markdown.
awesome :: Github.Token -> FilePath -> FilePath -> IO ()
awesome token src dst =
  Config.read src (writeCategoryToFile token dst)


-- Get the owner/repo info from the url.
githubInfo :: String -> Maybe (Github.RepoOwner, Github.RepoName)
githubInfo u =
  if length s < 2 then Nothing
  else Just (T.pack $ (last . init) s, T.pack $ last s)
  where s = splitOn "/" u


-- Fetch the information about Github repository if the link is a Github link.
githubRequest :: Github.Token -> String -> IO (Maybe MD.Link)
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


--
itemToLink :: Github.Token -> Config.Item -> IO (Maybe MD.Link)
itemToLink token (Config.Item u n d) =
  if Config.isGithub u then
    githubRequest token u
  else
    return $ n >>= (\title -> d <&> MD.ExternLink title u)


--
itemsToLinks :: Github.Token -> [Config.Item] -> IO [MD.Link]
itemsToLinks token =
  fmap (sortOn Down . catMaybes) . traverse (itemToLink token)


--
itemsToLinksAsync :: Github.Token -> [Config.Item] -> IO [Maybe MD.Link]
itemsToLinksAsync token = 
  Async.mapConcurrently (itemToLink token)


--
itemsToLinksAsync' :: Github.Token -> [Config.Item] -> IO [MD.Link]
itemsToLinksAsync' token = 
  fmap (sortOn Down . catMaybes) . itemsToLinksAsync token


--
writeCategoryToFile :: Github.Token -> FilePath -> Config.Category -> IO ()
writeCategoryToFile token file cat = do
  h <- openFile file WriteMode
  putStrLn "Write header..."
  hPutStr h $ showCategoryHeader cat
  putStrLn "Write Table Of Content..."
  hPutStr h "\n\n"
  forM_ (Config.categories cat) $ writeTOC h 0
  putStrLn "Write Categories..."
  forM_ (Config.categories cat) $ writeCategories token h 2
  hClose h


--
writeTOC :: Handle -> Int -> [Config.Category] -> IO()
writeTOC h level = mapM_ (\c ->
    let
      s = replicate (level*2) ' '
      t = Config.title (c :: Config.Category)
      l = fmap ( toLower .  (\x -> if x == ' ' then '-' else x)) t
      res = printf "%s- [%s](#%s)\n" s t l
    in
      hPutStr h res >> mapM_ (writeTOC h (level+1)) (Config.categories c))


--
writeCategories :: Github.Token -> Handle -> Int -> [Config.Category] -> IO ()
writeCategories token h l = 
  mapM_ (writeCategory token h l)


--
writeCategory :: Github.Token -> Handle -> Int -> Config.Category -> IO ()
writeCategory token h l c = do
  -- putStrLn $ "Category: " ++ Config.title (c :: Config.Category)
  hPutStr h $ showCategory l c
  forM_ (Config.items c) $ writeLinks h . itemsToLinksAsync' token
  forM_ (Config.categories c) $ writeCategories token h (l+1)


--
writeLinks :: Handle -> IO [MD.Link] -> IO ()
writeLinks h = 
  (=<<) $ mapM_ (B.hPut h . BC.pack . show)


--
showCategory :: Int -> Config.Category -> String
showCategory level (Config.Category t d _ _) =
  printf "\n\n%s %s\n\n%s" (replicate level '#') t dd
    where
      dd = maybe "" (\x -> if x == "" then "" else printf "*%s*\n\n" x) d :: String


--
showCategoryHeader :: Config.Category -> String
showCategoryHeader cat =
  printf "# %s\n\n%s" t d
    where
      t = Config.title (cat :: Config.Category)
      d = fromMaybe "" $ Config.description (cat :: Config.Category)


