{-# LANGUAGE OverloadedStrings #-}
module Data.Extracts.Parse (extracts) where

import           Control.Monad
import           Data.Bifunctor
import           Data.Either
import Data.Char
import           Data.Extracts
import           Data.Functor
import           Data.IntMap          (IntMap)
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Vector          as V
import           Data.Void
import           Text.Megaparsec
import           Text.Megaparsec.Char


type Parser = Parsec Void Text


extracts :: FilePath -> Text -> Either String Extracts
extracts path0 =
  first errorBundlePretty . parse (extractsP path0) path0


-------------------------------------------------------------------------------
-- | Parser Helpers


extractsP :: FilePath -> Parser Extracts
extractsP path0 = do
  xs <- (extractP `sepBy` some eol) <?> "extracts"
  eof
  return $ Extracts path0 xs

extractP :: Parser Extract
extractP =
  Extract <$> (V.fromList <$> some fragP) <?> "extract"

fragP :: Parser Fragment
fragP = try contentFragP <|> captureFragP

contentFragP :: Parser Fragment
contentFragP =
  Content <$> (takeWhile1P (Just "content") (\t -> t /= '[' && t /= '\n'))

captureFragP :: Parser Fragment
captureFragP =
  Capture <$> between (char '[') (char ']') (takeWhile1P (Just "capture") (\t -> t /= ']' && t /= '\n'))
