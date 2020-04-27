{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Data module for extracts
module Data.Extracts where

import           Data.Foldable
import           Data.Text     (Text)
import qualified Data.Text     as T
import           Data.Vector   (Vector)
import qualified Data.Vector   as V


-- | Represents a '.extracts' file
data Extracts = Extracts FilePath [Extract]
  deriving (Eq, Show)

-- | An extract
data Extract = Extract (Vector Fragment)
  deriving (Eq, Show)

-- | Sentence fragments
data Fragment
  = Content Text  -- ^ A sentence frament of raw textual content
  | Capture Text  -- ^ A sentence fragment captured between '[' ']'
  deriving (Eq, Show)

-- captures :: Extract -> [(Int, Text)]
-- captures (Extract frags0) = [(i, text0) | (i, Capture text0) <- V.toList (V.indexed frags0)]
--   where ifrags0 = elemIndices

-- blankout :: Extract -> Extract
-- blankout (Extract frags0) = Extract $ do
--   frag0 <- V.toList frags0
--   case frag0 of
--     Content txt -> return $ Content txt
--     Capture _   -> return $ Capture "_"


-- setCapture :: Int -> Text -> Extract -> Extract
-- setCapture index1 text1 (Extract frags0) = Extract $ do
--   (index0, frag0) <- V.toList (V.indexed frags0)
--   case frag0 of
--     Content txt -> return $ Content txt
--     Capture text0
--       | index0 == index1 -> return $ Capture text0
--       | otherwise -> return $ Capture text1

toText :: Extract -> Text
toText (Extract frags0) = fold (fragmentToText <$> frags0)

fragmentToText :: Fragment -> Text
fragmentToText (Content text0) = text0
fragmentToText (Capture text0) = text0