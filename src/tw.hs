import Control.Monad
import Data.Bifunctor
import Data.Char
import Data.Word
import System.Environment
import System.Exit

-- | Check whether a line ends with whitespace.
check :: (Word64, String)
      -> (Word64, Bool)
check = second (isSpace . last)

-- | Print a warning to the stdout with the line number.
warn :: (Word64, Bool)
     -> IO ()
warn (_, False)  = return ()
warn (num, True) = print num

-- | Parse command-line arguments.
getInput :: [String]  -- ^ arguments
         -> IO String -- ^ input file's content
getInput []       = getContents
getInput (path:_) = readFile path

-- | Take a decision based on a boolean value.
decide :: Bool -- ^ value
       -> a    -- ^ True option
       -> a    -- ^ False option
       -> a    -- ^ decision
decide True  x _ = x
decide False _ y = y

-- | Print numbers of lines that end with whitespace.
main :: IO ()
main = do
  args <- getArgs
  input <- getInput args
  let numbered = zip [1..] (lines input)
  let nonEmpty = filter (not . null . snd) numbered
  let trailing = map check nonEmpty
  mapM_ warn trailing
  decide (any snd trailing) exitFailure exitSuccess

