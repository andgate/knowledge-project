{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase       #-}

module Data.Extracts.Optic where

import           Control.Applicative
import qualified Data.DList                      as D
import           Data.Extracts
import           Data.Profunctor.Optic.Fold
import           Data.Profunctor.Optic.Lens
import           Data.Profunctor.Optic.Prism
import           Data.Profunctor.Optic.Traversal
import           Data.Profunctor.Optic.View
import           Data.Text                       (Text)
import           Data.Tuple.Optic
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V


-- class HasFragment s where
--   fragment :: Lens' s Text
--   content  :: Prism' s Text
--   capture :: Prism' s Text

-- class HasFragments s where
--   fragments :: Ixtraversal' Int s Fragment


-- instance HasFragment Fragment where
--   fragment =
--     lens
--       (\case
--         Content a -> a
--         Capture a -> a
--       )
--       (\s b -> case s of
--         Content _ -> Content b
--         Capture _ -> Capture b
--       )

--   content =
--     prism'
--       (\case
--         Content a -> Just a
--         Capture _ -> Nothing
--       )
--       Content

--   capture =
--     prism'
--       (\case
--         Content _ -> Nothing
--         Capture a -> Just a
--       )
--       Capture


-- instance HasFragments Extract where
--   fragments =
--     lens (\(Extract frags0) -> frags0) (\(Extract _) frags1 -> Extract frags1) . itraversed


-- captures :: HasFragments s => Ixtraversal' Int s (Text)
-- captures = fragments

-- -- content :: Fold Extract Text

-- -- clozes :: Fold Extract Text
-- -- captured = fold_ (\(Cloze _ fs) -> foldKeys D.empty)
-- --   where foldKeys ks = \case
-- --     Content _ -> f ks clz
-- --     Key     k -> f (D.snoc ks k) clz




