{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}

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
  , Trace
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
import qualified Data.Aeson.Types as AT

-- https://hackage.haskell.org/package/base
import Data.List (find, intercalate, isPrefixOf)
import Data.Maybe (fromMaybe, listToMaybe)
import Data.Version (showVersion)
import qualified System.Info

-- https://hackage.haskell.org/package/dlist
import qualified Data.DList as DList
import Data.DList (DList)

-- https://hackage.haskell.org/package/network-uri
import qualified Network.URI as URI

-- https://hackage.haskell.org/package/scientific
import qualified Data.Scientific as Sci

-- https://hackage.haskell.org/package/text
import qualified Data.Text as T

-- https://hackage.haskell.org/package/transformers
import Control.Monad.Trans.Writer (Writer, runWriter, tell)

-- (bm:cabal)
import qualified Paths_bm as Project

------------------------------------------------------------------------------
-- $Constants

version :: String
version = "bm-haskell " ++ showVersion Project.version

------------------------------------------------------------------------------

defaultCommand :: Command
defaultCommand = case System.Info.os of
    "mingw32" -> "start"
    "darwin"  -> "open"
    _other    -> "xdg-open"

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
type Trace = String
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
    keyword  <- parseToString =<< o .: "keyword"
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
      <*> (parseToString =<< o .: "value")

encodeParameter :: Parameter -> String
encodeParameter Parameter{..} = encodePart name ++ "=" ++ encodePart value
  where
    encodePart :: String -> String
    encodePart
      = map (\c -> if c == ' ' then '+' else c)
      . URI.escapeURIString ((||) <$> URI.isUnreserved <*> (== ' '))

------------------------------------------------------------------------------

data Proc
  = Proc
    { command   :: !Command
    , arguments :: ![Argument]
    }
  deriving Show

------------------------------------------------------------------------------
-- $API

run
  :: Config
  -> [Argument]
  -> (Either Error Proc, [Trace])
run Config{..} cliArgs = fmap DList.toList . runWriter $ do
    trace $ formatCommand configCommand
    loop configCommand configArgs cliArgs
  where
    loop
      :: Command
      -> [Bookmark]
      -> [Argument]
      -> Writer (DList Trace) (Either Error Proc)
    loop cmd bms (arg:args) = case find (isPrefixOf arg . keyword) bms of
      Just bm -> do
        trace $ formatBookmark bm
        case queryOrArgs bm of
          Left query
            | null args -> case mUrl bm of
                Just url -> openUrl (fromMaybe cmd $ mCommand bm) url
                Nothing -> returnError $ "no query for " ++ keyword bm
            | otherwise -> openQuery (fromMaybe cmd $ mCommand bm) query args
          Right bms'
            | null args -> case mUrl bm of
                Just url -> openUrl (fromMaybe cmd $ mCommand bm) url
                Nothing  -> case listToMaybe bms' of
                  Just bm' ->
                    loop (fromMaybe cmd $ mCommand bm) bms' [keyword bm']
                  Nothing -> returnError $ "no URL for " ++ keyword bm
            | otherwise -> loop (fromMaybe cmd $ mCommand bm) bms' args
      Nothing -> returnError $ "unknown argument: " ++ arg
    loop _cmd _bms [] = returnError "no arguments"

    returnError :: Error -> Writer (DList Trace) (Either Error Proc)
    returnError = return . Left

    openUrl :: Command -> Url -> Writer (DList Trace) (Either Error Proc)
    openUrl cmd url = do
      trace $ unwords [cmd, url]
      return . Right $ Proc cmd [url]

    openQuery
      :: Command
      -> Query
      -> [Argument]
      -> Writer (DList Trace) (Either Error Proc)
    openQuery cmd Query{..} args
      = openUrl cmd
      . (action ++)
      . ('?' :)
      . intercalate "&"
      . map encodeParameter
      $ Parameter parameter (unwords args) : hiddenParameters

    trace :: Trace -> Writer (DList Trace) ()
    trace = tell . DList.singleton

    formatCommand :: Command -> Trace
    formatCommand = ('[' :) . (++ "]")

    formatKeyword :: Keyword -> Trace
    formatKeyword = ('<' :) . (++ ">")

    formatBookmark :: Bookmark -> Trace
    formatBookmark Bookmark{..} = case mCommand of
      Just command -> unwords [formatKeyword keyword, formatCommand command]
      Nothing      -> formatKeyword keyword

------------------------------------------------------------------------------
-- $Internal

-- | Parse any scalar value as a string
--
-- Strings, numbers, booleans, and null are parsed as a string.  Empty
-- strings, arrays, and objects result in an error.
parseToString :: A.Value -> AT.Parser String
parseToString = \case
    (A.String t)  -> pure $ T.unpack t
    (A.Number n)  -> pure . either (show @Double) (show @Integer) $
      Sci.floatingOrInteger n
    (A.Bool b)    -> pure $ if b then "true" else "false"
    A.Null        -> pure "null"
    A.Array{}     -> fail "unexpected array"
    A.Object{}    -> fail "unexpected object"
