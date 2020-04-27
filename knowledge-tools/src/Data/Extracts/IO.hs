module Data.Extracts.IO where

import           Control.Monad
import           Data.Extracts
import qualified Data.Extracts.Parse as Parse
import qualified Data.Text        as T
import qualified Data.Text.IO     as T


-- | Read a notes in a text file as a clozed module,
--     clozing over the file line-by-line.
readExtracts :: FilePath -> IO Extracts
readExtracts path0 = do
  content0 <- T.readFile path0
  case Parse.extracts path0 content0 of
    Left err -> error err
    Right extracts0 -> return extracts0