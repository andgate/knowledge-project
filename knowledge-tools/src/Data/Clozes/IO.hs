module Data.Clozes.IO where


import Data.Clozes
import qualified Data.Text.IO as T


writeClozes :: DefnSym -> ClozeSym -> Clozes -> IO ()
writeClozes defnSym clozeSym clozes0@(Clozes path0 _) =
  T.writeFile path0 (clozesToText defnSym clozeSym clozes0)