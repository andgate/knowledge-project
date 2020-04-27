{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Data.Clozes
import           Data.Clozes.IO
import           Data.Extracts
import           Data.Extracts.IO
import qualified Data.Extracts.Parse        as P
import           Safe                       (headDef)
import           System.Directory
import           System.Directory.Recursive
import           System.Environment
import           System.FilePath

main :: IO ()
main = do
  args0 <- getArgs
  let path0 = headDef "./" args0

  paths0 <- filter (\p -> hasExtension p && isExtensionOf "extracts" p) <$> getDirRecursive path0

  extractFiles0 <- mapM readExtracts paths0
  let clozeFiles0 = fromExtracts <$> extractFiles0
  mapM_ (writeClozes "\t" "___") clozeFiles0
