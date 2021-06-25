{-# LANGUAGE RecordWildCards #-}

module Main (main) where

-- https://hackage.haskell.org/package/base
import Control.Applicative (optional, some)
import Control.Monad (when)
import System.Exit (ExitCode(ExitFailure), exitWith)
import System.IO (hPutStrLn, stderr)

-- https://hackage.haskell.org/package/directory
import qualified System.Directory as Dir

-- https://hackage.haskell.org/package/optparse-applicative
import qualified Options.Applicative as OA

-- https://hackage.haskell.org/package/typed-process
import qualified System.Process.Typed as TP

-- https://hackage.haskell.org/package/yaml
import qualified Data.Yaml as Yaml

-- (bm)
import qualified BM

-- (bm:executable)
import qualified LibOA

------------------------------------------------------------------------------
-- $Options

data Options
  = Options
    { configOpt :: !(Maybe FilePath)
    , traceOpt  :: !Bool
    , args      :: ![String]
    }
  deriving Show

configOption :: OA.Parser FilePath
configOption = OA.strOption $ mconcat
    [ OA.long "config"
    , OA.short 'c'
    , OA.metavar "CONFIG"
    , OA.help "config file (default: $XDG_CONFIG_HOME/bm.yaml)"
    ]

traceOption :: OA.Parser Bool
traceOption = OA.switch $ mconcat
    [ OA.long "trace"
    , OA.short 't'
    , OA.help "show trace output for debugging"
    ]

arguments :: OA.Parser [String]
arguments = some . OA.strArgument $ mconcat
    [ OA.metavar "ARG [ARG ...]"
    , OA.help "arguments"
    ]

options :: OA.Parser Options
options = Options
    <$> optional configOption
    <*> traceOption
    <*> arguments

------------------------------------------------------------------------------
-- $Main

errorExit :: String -> IO a
errorExit message = do
    hPutStrLn stderr $ "error: " ++ message
    exitWith $ ExitFailure 1

runProc :: BM.Proc -> IO ()
runProc proc
    = TP.runProcess_
    . TP.setStdin TP.nullStream
    . TP.setStdout TP.nullStream
    . TP.setStderr TP.nullStream
    $ TP.proc (BM.command proc) (BM.arguments proc)

main :: IO ()
main = do
    Options{..} <- OA.execParser pinfo
    configPath <- case configOpt of
      Just path -> pure path
      Nothing -> Dir.getXdgDirectory Dir.XdgConfig "bm.yaml"
    config <- Yaml.decodeFileThrow configPath
    let (eep, traceLines) = BM.run config args
    when traceOpt $ mapM_ (hPutStrLn stderr) traceLines
    either errorExit runProc eep
  where
    pinfo :: OA.ParserInfo Options
    pinfo
      = OA.info (LibOA.helper <*> LibOA.versioner BM.version <*> options)
      $ mconcat
          [ OA.fullDesc
          , OA.progDesc
              "open bookmarks and search queries from the command line"
          , OA.failureCode 2
          ]
