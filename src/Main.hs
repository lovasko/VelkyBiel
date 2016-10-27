{-# LANGUAGE OverloadedStrings #-}

import Data.Bifunctor
import Data.Char
import Data.Monoid
import Data.Word
import System.Environment
import System.Exit

import qualified Data.Text.Lazy as T
import qualified Data.Text.Lazy.IO as T

-- | File entry defined by it's name and content.
data Entry = Entry T.Text T.Text

-- | Check whether a line ends with whitespace.
check :: (Word64, T.Text) -- ^ numbered line
      -> (Word64, Bool)      -- ^ numbered decision
check = second (isSpace . T.last)

-- | Print a warning to the stdout with the line number.
warn :: T.Text      -- ^ file path
     -> (Word64, Bool) -- ^ line decision
     -> IO ()          -- ^ action
warn _    (_, False)  = return ()
warn path (num, True) = T.putStrLn $ path <> ": " <> T.pack (show num)

-- | Parse command-line arguments.
getEntries :: [String]   -- ^ arguments
           -> IO [Entry] -- ^ file entries
getEntries []    = do
  content <- T.getContents
  return [Entry "<stdin>" content]
getEntries paths = do
  contents <- mapM T.readFile paths
  return $ zipWith Entry (map T.pack paths) contents

-- | Take a decision based on a boolean value.
decide :: Bool -- ^ value
       -> a    -- ^ True option
       -> a    -- ^ False option
       -> a    -- ^ decision
decide True  x _ = x
decide False _ y = y

-- | Process a single file entry - determine the trailing whitespace
-- property and print an appropriate warning to the stdout.
processEntry :: Entry   -- ^ entry
             -> IO Bool -- ^ trailing decision
processEntry (Entry path content) = do
  let numbered = zip [1..] (T.lines content)
  let nonEmpty = filter (not . T.null . snd) numbered
  let trailing = map check nonEmpty
  mapM_ (warn path) trailing
  return $ any snd trailing

-- | Print numbers of lines that end with whitespace.
main :: IO ()
main = do
  entries  <- getArgs >>= getEntries
  trailing <- mapM processEntry entries
  decide (or trailing) exitFailure exitSuccess

