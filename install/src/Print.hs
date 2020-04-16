module Print where

import           Control.Monad.IO.Class
import           Data.List                                ( dropWhileEnd
                                                          )
import           Data.Char                                ( isSpace )

-- | print a line prepended with 4 spaces
putStrLnIndented :: String -> IO ()
putStrLnIndented = putStrLn . ("    " ++)

embedInStars :: String -> String
embedInStars str =
  let starsLine = "\n" <> replicate 80 '*' <> "\n"
  in  starsLine <> str <> starsLine

printInStars :: String -> IO ()
printInStars = putStrLn . embedInStars


-- | Trim whitespace of both ends of a string
trim :: String -> String
trim = dropWhileEnd isSpace . dropWhile isSpace

-- | Trim the whitespace of the stdout of a command
trimmedStdout :: String -> String
trimmedStdout s = trim s

type TargetDescription = (String, String)

-- | Number of spaces the target name including whitespace should have.
-- At least twenty, maybe more if target names are long. At most the length of the longest target plus five.
space :: [(String,String)] -> Int
space helpItems = maximum (20 : map ((+ 5) . length . fst) helpItems)

-- | Show a target.
-- Concatenates the target with its help message and inserts whitespace between them.
showHelpItem :: Int -> (String,String) -> String
showHelpItem spaces (helpItemKey, msg) =
  helpItemKey ++ replicate (spaces - length helpItemKey) ' ' ++ msg
