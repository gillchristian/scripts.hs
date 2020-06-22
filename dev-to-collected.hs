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

{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

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
import Network.Wreq
import qualified Options.Applicative as Opt
import Options.Applicative ((<**>))
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.Text

type DevtoArticle =
  ( Text, -- title
    Text, -- published
    Text, -- cover_image
    Text, -- url
    Text -- body
  )

eitherToMaybe :: Either e a -> Maybe a
eitherToMaybe (Right a) = Just a
eitherToMaybe _ = Nothing

stripMeta :: Parser Text
stripMeta = do
  _ <- spaces
  _ <- string "--"
  _ <- spaces
  _ <- manyTill anyChar $ try $ string "---"
  _ <- spaces
  Text.pack <$> many anyChar

devtoToCollected :: DevtoArticle -> Either ParseError (Text, Text)
devtoToCollected (title, _, cover_image, url, body_markdown) =
  makeArticle <$> parse stripMeta "" body_markdown
  where
    makeArticle body =
      ( title,
        Text.unlines
          [ "# " <> title <> "\n",
            "*Originally posted on [dev.to/gillchristian](" <> url <> ")*.\n",
            "![" <> title <> "](" <> cover_image <> ")\n",
            body
          ]
      )

extract :: Response LazyBS.ByteString -> [DevtoArticle]
extract r =
  let Fold article =
        (,,,,) <$> Fold (key "title" . _String)
          <*> Fold (key "published_at" . _String)
          <*> Fold (key "cover_image" . _String)
          <*> Fold (key "url" . _String)
          <*> Fold (key "body_markdown" . _String)
   in r ^.. responseBody . values . article

snd5 :: (a, b, c, d, e) -> b
snd5 (_, b, _, _, _) = b

processPosts :: Response LazyBS.ByteString -> [(Text, Text)]
processPosts = mapMaybe eitherToMaybe . fmap devtoToCollected . sortOn snd5 . extract

getDevtoArticles :: Config -> IO (Response LazyBS.ByteString)
getDevtoArticles Config {..} =
  getWith reqConfg url
  where
    -- curl -H "api-key: API_KEY" https://dev.to/api/articles/me/published
    url = "https://dev.to/api/articles/me/published"
    reqConfg = defaults & header "api-key" .~ [devtoApiKey]

postCollectedArticle :: Config -> (Text, Text) -> IO ()
postCollectedArticle Config {..} (title, postBody) = do
  putStrLn $ Text.unpack $ "Posting: " <> title
  r <- postWith reqConfg url body
  putStrLn $ Text.unpack $ successMsg r
  where
    -- curl -H "Authorization: your@email.com your-secret-token" \
    -- -H "Accept: application/json" \
    -- -H "Content-Type: application/json" \
    -- https://collectednotes.com/sites/1/notes \
    -- -d '{"note": {"body": "# My new note title\nThis is the body", "visibility": "private"}}'
    reqConfg =
      defaults
        & header "Authorization" .~ [email <> " " <> collectednotesToken]
        & header "Content-Type" .~ ["application/json"]
        & header "Accept" .~ ["application/json"]
    url = "https://collectednotes.com/sites/" <> siteId <> "/notes"
    body = object ["note" .= object ["body" .= postBody, "visibility" .= ("private" :: Text)]]
    successMsg r =
      "✓ Note created: ("
        <> fromMaybe "<no response body>" (r ^? responseBody . key "url" . _String)
        <> ")\n"

data Config = Config
  { siteId :: String,
    email :: BS.ByteString,
    collectednotesToken :: BS.ByteString,
    devtoApiKey :: BS.ByteString
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

opts :: Opt.ParserInfo Config
opts =
  Opt.info
    (configP <**> Opt.helper)
    ( Opt.fullDesc
        <> Opt.header "Script to migrate all your dev.to articles to CollectedNotes"
    )

main :: IO ()
main = do
  config <- Opt.execParser opts
  r <- getDevtoArticles config
  mapM_ (postCollectedArticle config) $ processPosts r
  putStrLn "Great success!!! 🎉"
