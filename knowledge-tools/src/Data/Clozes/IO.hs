module Data.Clozes.IO where


import Data.Clozes
import qualified Data.Text.IO as T


writeClozes :: Clozes -> IO ()
writeClozes clopzes0@(Clozes path0 _) =
  T.writeFile path0 (clozesToText clozes0)