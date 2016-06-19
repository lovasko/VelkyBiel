import Data.Bifunctor
import Data.Char
import Data.Word
import System.Environment
import System.Exit
import qualified Data.Text.Lazy as Text
import qualified Data.Text.Lazy.IO as Text

-- | Check whether a line ends with whitespace.
check :: (Word64, Text.Text)
      -> (Word64, Bool)
check = second (isSpace . Text.last)

-- | Print a warning to the stdout with the line number.
warn :: (Word64, Bool)
     -> IO ()
warn (_, False)  = return ()
warn (num, True) = print num

-- | Parse command-line arguments.
getInput :: [String]     -- ^ arguments
         -> IO Text.Text -- ^ input file's content
getInput []       = Text.getContents
getInput (path:_) = Text.readFile path

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
  let numbered = zip [1..] (Text.lines input)
  let nonEmpty = filter (not . Text.null . snd) numbered
  let trailing = map check nonEmpty
  mapM_ warn trailing
  decide (any snd trailing) exitFailure exitSuccess

