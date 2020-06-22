#!/usr/bin/env stack
{- stack
   script
   --resolver lts-13.27
   --package aeson
   --package sort
   --package lens-aeson
   --package wreq
   --package lens
   --package text
   --package bytestring
   --package parsec
   --package optparse-applicative
-}

{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative (optional)
import Control.Lens hiding ((.=))
import Data.Aeson
import Data.Aeson.Lens
import qualified Data.ByteString.Internal as BS
import qualified Data.ByteString.Lazy.Internal as LazyBS
import Data.Maybe
import Data.Ord
import Data.Sort
import qualified Data.Text as Text
import Data.Text (Text)
import GHC.Generics
import Network.Wreq
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import Text.Parsec hiding (optional)
import Text.Parsec.Char
import Text.Parsec.Text

data DevtoArticle = DevtoArticle
  { title :: Text,
    published :: Bool,
    main_image :: Text,
    canonical_url :: Text,
    tags :: Maybe [Text],
    series :: Maybe Text,
    body_markdown :: Text
  }
  deriving (Show, Generic, ToJSON)

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe _ = Nothing

stripTitleAndFirstImage :: Parser Text
stripTitleAndFirstImage = do
  _ <- spaces
  _ <- manyTill anyChar $ try newline
  _ <- spaces
  before <- manyTill anyChar $ try $ string "!["
  _ <- manyTill anyChar $ try $ char ')'
  Text.pack . (before <>) <$> many anyChar

collectedToDev :: Config -> CollectedArticle -> Either ParseError DevtoArticle
collectedToDev Config {..} (title, body, _, url, image, _) =
  DevtoArticle title optPublished image url tags' optSeries . Text.strip
    <$> parse stripTitleAndFirstImage "" body
  where
    tags' = fmap (fmap Text.strip . Text.splitOn ",") optTags

sixth :: (a, b, c, d, e, f) -> f
sixth (_, _, _, _, _, f) = f

processPosts :: Config -> Response LazyBS.ByteString -> [DevtoArticle]
processPosts config =
  mapMaybe eitherToMaybe . fmap (collectedToDev config) . findNoteToPublish . extract
  where
    findNoteToPublish = case optNote config of
      Just note -> filter ((note ==) . sixth)
      Nothing -> take 1

type CollectedArticle =
  ( Text, -- title
    Text, -- body
    Text, -- visibility ("public" | "private")
    Text, -- url
    Text, -- poster (picture)
    Text -- path (ie. slug)
  )

extract :: Response LazyBS.ByteString -> [CollectedArticle]
extract r =
  let Fold article =
        (,,,,,) <$> Fold (key "title" . _String)
          <*> Fold (key "body" . _String)
          <*> Fold (key "visibility" . _String)
          <*> Fold (key "url" . _String)
          <*> Fold (key "poster" . _String)
          <*> Fold (key "path" . _String)
   in r ^.. responseBody . values . article

getCollectedArticles :: Config -> IO (Response LazyBS.ByteString)
getCollectedArticles Config {..} =
  getWith reqConfg url
  where
    -- curl -H "Authorization: your@email.com your-secret-token" \
    -- -H "Accept: application/json" \
    -- -H "Content-Type: application/json" \
    -- https://collectednotes.com/sites/1/notes
    reqConfg =
      defaults
        & header "Authorization" .~ [email <> " " <> collectednotesToken]
        & header "Content-Type" .~ ["application/json"]
        & header "Accept" .~ ["application/json"]
    url = "https://collectednotes.com/sites/" <> siteId <> "/notes"

postDevtoArticle :: Config -> DevtoArticle -> IO ()
postDevtoArticle Config {..} article = do
  putStrLn $ Text.unpack $ "Posting: " <> title article
  r <- postWith reqConfg url body
  putStrLn $ Text.unpack $ successMsg r
  where
    -- curl -X POST -H "Content-Type: application/json" \
    --   -H "api-key: API_KEY" \
    --   -d '{"article":{"title":"Title","body_markdown":"Body","published":false,"tags":["discuss", "javascript"]}}' \
    --   https://dev.to/api/articles
    reqConfg =
      defaults
        & header "api-key" .~ [devtoApiKey]
        & header "Content-Type" .~ ["application/json"]
        & header "Accept" .~ ["application/json"]
    url = "https://dev.to/api/articles"
    body = object ["article" .= article]
    successMsg r =
      "✓ dev.to post created: ("
        <> fromMaybe "<no response body>" (r ^? responseBody . key "url" . _String)
        <> ")\n"

data Config = Config
  { siteId :: String,
    email :: BS.ByteString,
    collectednotesToken :: BS.ByteString,
    devtoApiKey :: BS.ByteString,
    optPublished :: Bool,
    optTags :: Maybe Text,
    optSeries :: Maybe Text,
    optNote :: Maybe Text
  }
  deriving (Show)

configP :: Opt.Parser Config
configP =
  Config
    <$> Opt.strOption
      ( Opt.long "site-id"
          <> Opt.metavar "SITE_ID"
          <> Opt.help "CollectedNotes notes site ID"
      )
    <*> Opt.strOption
      ( Opt.long "email"
          <> Opt.metavar "EMAIL"
          <> Opt.help "Your CollectedNotes email"
      )
    <*> Opt.strOption
      ( Opt.long "token"
          <> Opt.metavar "TOKEN"
          <> Opt.help "CollectedNotes API Token"
      )
    <*> Opt.strOption
      ( Opt.long "api-key"
          <> Opt.metavar "API_KEY"
          <> Opt.help "dev.to API Key"
      )
    <*> Opt.switch
      ( Opt.long "publish"
          <> Opt.help "Publish the article (creates as a draft by default)"
          <> Opt.showDefault
      )
    <*> optional
      ( Opt.strOption
          ( Opt.long "tags"
              <> Opt.metavar "TAGS"
              <> Opt.help "Comma separated list of tags (max 4)"
          )
      )
    <*> optional
      ( Opt.strOption
          ( Opt.long "series"
              <> Opt.metavar "SERIES"
              <> Opt.help "dev.to article series"
          )
      )
    <*> optional
      ( Opt.strOption
          ( Opt.long "note"
              <> Opt.metavar "NOTE_SLUG"
              <> Opt.help "The path of the note to publish (collectednotes.com/my-site/<NOTE_SLUG>). If missing the latest note will be published"
          )
      )

opts :: Opt.ParserInfo Config
opts =
  Opt.info
    (configP <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.header "Post a note from CollectedNotes to dev.to"
    )

main :: IO ()
main = do
  config <- Opt.execParser opts
  r <- getCollectedArticles config
  mapM_ (postDevtoArticle config) $ processPosts config r
