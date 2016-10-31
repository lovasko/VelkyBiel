import Data.Char
import System.Environment
import System.Exit

import qualified Data.ByteString.Lazy.Char8 as C

-- | File entry defined by its name and content.
data Entry = Entry
             String       -- ^ name
             C.ByteString -- ^ content

-- | Check whether a line ends with whitespace.
check :: (Integer, C.ByteString) -- ^ number & line
      -> (Integer, Bool)         -- ^ number & decision
check (num, line) = (num, (isSpace . C.last) line)

-- | Print a warning to the stdout with the line number.
warn :: String          -- ^ file path
     -> (Integer, Bool) -- ^ line decision
     -> IO ()           -- ^ action
warn _    (_,   False)  = return ()
warn path (num, True)   = putStrLn $ path ++ ": " ++ show num

-- | Parse command-line arguments.
getEntries :: [String]   -- ^ arguments
           -> IO [Entry] -- ^ file entries
getEntries []    = do
  content <- C.getContents
  return [Entry "<stdin>" content]
getEntries paths = do
  contents <- mapM C.readFile paths
  return $ zipWith Entry paths contents

-- | Process a single file entry - determine the trailing whitespace
-- property and print an appropriate warning to the stdout.
processEntry :: Entry   -- ^ entry
             -> IO Bool -- ^ trailing decision
processEntry (Entry path content) = do
  let numbered = zip [1..] (C.lines content)
  let nonEmpty = filter (not . C.null . snd) numbered
  let trailing = map check nonEmpty
  mapM_ (warn path) trailing
  return $ any snd trailing

-- | Print numbers of lines and corresponding filenames that contain
-- trailing whitespace. The utility expects zero or more files as its
-- arguments. In case of zero arguments, the stdin stream is used.
main :: IO ()
main = do
  entries  <- getArgs >>= getEntries
  trailing <- mapM processEntry entries
  if or trailing then exitFailure else exitSuccess

