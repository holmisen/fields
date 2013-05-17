{-# LANGUAGE NoMonomorphismRestriction #-}

-- Arrange tabular data in columns.
--
-- Input data is pipe separated fields read from stdin.


import Data.List   (intersperse, transpose)
import Text.Printf (printf)
import Text.Regex  (splitRegex, mkRegex)


-- Input field separator regexp
inputFieldSeparatorRE = "\\|"

-- Output field separator
outputFieldSeparator  = "  "


splitColumns = splitRegex (mkRegex inputFieldSeparatorRE)

rowsToColumns = transpose
columnsToRows = transpose


formatColumns cs = map (uncurry formatColumn) (zip sizes cs)  where
    sizes = map columnSize cs

formatColumn n xs = map (formatCell n) xs

formatCell = printf "%-*s"

columnSize = maximum . map length

mkLine = concat . intersperse outputFieldSeparator


main = do
  ls <- lines `fmap` getContents
  let rs  = map splitColumns ls
  let rs' = columnsToRows $ formatColumns $ rowsToColumns rs
  let ls' = map mkLine rs'
  mapM_ putStrLn ls'
