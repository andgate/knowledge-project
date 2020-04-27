{-# LANGUAGE OverloadedStrings #-}

module Data.Clozes where

import           Data.Extracts
import           Data.Extracts.Optic
import           Data.Foldable
import           Data.IntSet                     (IntSet)
import qualified Data.IntSet                     as IntSet
import           Data.Profunctor.Optic.Prelude
import           Data.Profunctor.Optic.Traversal
import           Data.Text                       (Text)
import qualified Data.Text                       as T
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import           System.FilePath

data Clozes = Clozes FilePath [Cloze]

-- | A single cloze is some content which can be inserted into a string with blanks
-- data Cloze = Cloze Text (Text -> Text)

data Cloze = Cloze IntSet (Vector Text)

emptyCloze :: Cloze
emptyCloze = Cloze mempty mempty

fromExtracts :: Extracts -> Clozes
fromExtracts (Extracts path0 extracts0)
  = Clozes path1 (fromExtract <$> extracts0)
  where path1 = replaceExtension path0 "clozes"

fromExtract :: Extract -> Cloze
fromExtract (Extract frags0)
  = V.foldl builder0 emptyCloze (V.indexed frags0)
  where
    builder0 (Cloze indices0 chunks0) (i, frag0) =
      case frag0 of
        Content t -> Cloze indices0 (V.snoc chunks0 t)
        Capture t -> Cloze (IntSet.insert i indices0) (V.snoc chunks0 t)


type DefnSym = Text
type ClozeSym = Text

toTextForms :: DefnSym -> ClozeSym -> Cloze -> [Text]
toTextForms defnSym clozeSym (Cloze indices0 chunks0) =
  (\i -> (chunks0 V.! i) <> defnSym <> fold (chunks0 V.// [(i, clozeSym)]))
    <$> (IntSet.toList indices0)


clozesToText :: DefnSym -> ClozeSym -> Clozes -> Text
clozesToText defnSym clozeSym (Clozes path0 clozes)
  = T.unlines $ foldr (<>) mempty (toTextForms defnSym clozeSym <$> clozes)

-- clozeAll :: Extract -> [Cloze]

-- clozeSingle :: Text -> Extract -> Cloze

-- clozeOn :: Extract -> [Cloze]
-- clozeOn (Extract frags0) = foldr f [] frags0

-- fromExtract :: Extract -> [Cloze]
-- fromExtract xtract0 = Cloze captures0 clozure0
--   where captures0 = V.fromList $ xtract0 ^.. fragments . capture
--         clozure0 = \captures1 -> foldr builder0 mempty captures1
--         builder0 capture1 text0 =
--           text0 ,. toText $ (xtract0 & ix fragments %%~ (\i frag0 ->
--             let capture0 = (frag0 ^. fragment)
--             in if capture0 /= capture1 then capture1 else capture0))
