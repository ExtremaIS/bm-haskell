{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module BM
  ( -- * Constants
    version
    -- * Types
  , Action
  , Argument
  , Command
  , Error
  , Keyword
  , ParameterName
  , ParameterValue
  , Url
  , Config(..)
  , Bookmark(..)
  , Query(..)
  , Parameter(..)
  , Proc(..)
    -- * API
  , run
  ) where

-- https://hackage.haskell.org/package/aeson
import qualified Data.Aeson as A
import Data.Aeson (FromJSON, (.:), (.:?), (.!=))

-- https://hackage.haskell.org/package/base
import Data.List (find, intercalate, isPrefixOf)
import Data.Maybe (fromMaybe)
import Data.Version (showVersion)

-- https://hackage.haskell.org/package/network-uri
import qualified Network.URI as URI

-- (bm:cabal)
import qualified Paths_bm as Project

------------------------------------------------------------------------------
-- $Constants

version :: String
version = "bm-haskell " ++ showVersion Project.version

------------------------------------------------------------------------------

defaultCommand :: Command
defaultCommand = "xdg-open"

defaultParameter :: ParameterName
defaultParameter = "q"

------------------------------------------------------------------------------
-- $Types

type Action = String
type Argument = String
type Command = FilePath
type Error = String
type Keyword = String
type ParameterName = String
type ParameterValue = String
type Url = String

------------------------------------------------------------------------------

data Config
  = Config
    { configCommand :: !Command
    , configArgs    :: ![Bookmark]
    }
  deriving Show

instance FromJSON Config where
  parseJSON = A.withObject "Config" $ \o ->
    Config
      <$> o .:? "command" .!= defaultCommand
      <*> o .:  "args"

------------------------------------------------------------------------------

data Bookmark
  = Bookmark
    { keyword     :: !Keyword
    , mCommand    :: !(Maybe Command)
    , mUrl        :: !(Maybe Url)
    , queryOrArgs :: !(Either Query [Bookmark])
    }
  deriving Show

instance FromJSON Bookmark where
  parseJSON = A.withObject "Bookmark" $ \o -> do
    keyword  <- o .:  "keyword"
    mCommand <- o .:? "command"
    mUrl     <- o .:? "url"
    mQuery   <- o .:? "query"
    mArgs    <- o .:? "args"
    queryOrArgs <- case (mQuery, mArgs) of
      (Nothing,    Just args) -> pure $ Right args
      (Just query, Nothing)   -> pure $ Left query
      (Nothing,    Nothing)   -> pure $ Right []
      (Just{},     Just{})    -> fail $
        "bookmark has both query and args: " ++ keyword
    pure Bookmark{..}

------------------------------------------------------------------------------

data Query
  = Query
    { action           :: !Action
    , parameter        :: !ParameterName
    , hiddenParameters :: ![Parameter]
    }
  deriving Show

instance FromJSON Query where
  parseJSON = A.withObject "Query" $ \o ->
    Query
      <$> o .:  "action"
      <*> o .:? "parameter" .!= defaultParameter
      <*> o .:? "hidden"    .!= []

------------------------------------------------------------------------------

data Parameter
  = Parameter
    { name  :: !ParameterName
    , value :: !ParameterValue
    }
  deriving Show

instance FromJSON Parameter where
  parseJSON = A.withObject "Parameter" $ \o ->
    Parameter
      <$> o .: "name"
      <*> o .: "value"

encodeParameter :: Parameter -> String
encodeParameter Parameter{..} =
    name ++ "=" ++ URI.escapeURIString URI.isUnreserved value

------------------------------------------------------------------------------

data Proc
  = Proc
    { command   :: !Command
    , arguments :: ![Argument]
    }
  deriving Show

------------------------------------------------------------------------------
-- $API

run :: Config -> [Argument] -> Either Error Proc
run Config{..} = loop configCommand configArgs
  where
    loop :: Command -> [Bookmark] -> [Argument] -> Either Error Proc
    loop cmd bms (arg:args) = case find (isPrefixOf arg . keyword) bms of
      Just Bookmark{..} -> case queryOrArgs of
        Left query
          | null args -> case mUrl of
              Just url -> Right $ openUrl (fromMaybe cmd mCommand) url
              Nothing  -> Left $ "no query for " ++ keyword
          | otherwise ->
              Right $ openQuery (fromMaybe cmd mCommand) query args
        Right bms'
          | null args -> case mUrl of
              Just url -> Right $ openUrl (fromMaybe cmd mCommand) url
              Nothing  -> Left $ "no URL for " ++ keyword
          | otherwise -> loop (fromMaybe cmd mCommand) bms' args
      Nothing -> Left $ "unknown argument: " ++ arg
    loop _cmd _bms [] = Left "no arguments"

    openUrl :: Command -> Url -> Proc
    openUrl cmd url = Proc cmd [url]

    openQuery :: Command -> Query -> [Argument] -> Proc
    openQuery cmd Query{..} args
      = openUrl cmd
      . (action ++)
      . ('?' :)
      . intercalate "&"
      . map encodeParameter
      $ Parameter parameter (unwords args) : hiddenParameters
