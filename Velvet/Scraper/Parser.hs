{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}
{-# OPTIONS_GHC -fno-warn-unused-do-bind #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Velvet.Scraper.Parser (VelvetInfo, parseLogFile, viHeader) where

import Control.Applicative
import Data.Attoparsec.Text as Attoparsec
import Data.Csv (FromRecord, ToRecord, ToNamedRecord, Header)
import Data.Text (Text)
import qualified Data.Vector as Vector (fromList)
import GHC.Generics

data VelvetInfo = VelvetInfo {
    assembly_id                    :: Text
  , exp_cov                        :: Int
  , cov_cutoff                     :: Double
  , hash_value                     :: Int
  , total_contigs                  :: Int
  , n50                            :: Int
  , longest_contig_length          :: Int
  , total_bases                    :: Int
  , num_contigs_over_1k            :: Int
  , total_bases_in_contigs_over_1k :: Int
  } deriving (Show, Generic)

instance FromRecord VelvetInfo

instance ToRecord VelvetInfo

instance ToNamedRecord VelvetInfo

viHeader :: Header
viHeader = Vector.fromList [
    "assembly_id"
  , "exp_cov"
  , "cov_cutoff"
  , "hash_value"
  , "total_contigs"
  , "n50"
  , "longest_contig_length"
  , "total_bases"
  , "num_contigs_over_1k"
  , "total_bases_in_contigs_over_1k"
  ]

data CovInfo = CovInfo {
    expCov    :: Int
  , cutoffCov :: Double
  } deriving Show

lexeme :: Parser a -> Parser a
lexeme p = p <* many1 space

field :: Text -> Parser a -> Parser a
field s p =
  string s *> lexeme (char ':') *> p <* takeTill isEndOfLine <* endOfLine

uninteresting :: Parser Text
uninteresting = takeTill isEndOfLine <* endOfLine

skipTill :: Alternative f => f a -> f b -> f b
skipTill skippedAction nextAction =
      nextAction
  <|> skippedAction *> skipTill skippedAction nextAction

finalHeader :: Parser ()
finalHeader = string "Final optimised assembly details:" *> endOfLine

parameters :: Parser CovInfo
parameters = do
  string "Velvetg parameter string"
  lexeme (char ':')
  string "auto_data_" *> lexeme decimal *> lexeme (string "-clean yes")
  expCov <- lexeme (string "-exp_cov") *> lexeme decimal
  cutoffCov <- lexeme (string "-cov_cutoff") *> lexeme double
  return CovInfo {..}

interesting :: Parser VelvetInfo
interesting = do
  count 9 uninteresting
  CovInfo exp_cov cov_cutoff <- parameters
  uninteresting
  hash_value <- field "Velvet hash value" decimal
  uninteresting
  total_contigs <- field "Total number of contigs" decimal
  n50 <- field "n50" decimal
  longest_contig_length <- field "length of longest contig" decimal
  total_bases <- field "Total bases in contigs" decimal
  num_contigs_over_1k <- field "Number of contigs > 1k" decimal
  total_bases_in_contigs_over_1k <- field "Total bases in contigs > 1k" decimal
  let assembly_id = ""
  return VelvetInfo {..}

assemblyOutputFileHeader :: Parser ()
assemblyOutputFileHeader =
  string "Assembly output files are in the following directory:" *> endOfLine

logFile :: Parser VelvetInfo
logFile = do
  uninteresting `skipTill` finalHeader
  velvet <- interesting
  uninteresting `skipTill` assemblyOutputFileHeader
  label <- takeTill (== 'P') *> takeTill isEndOfLine
  return $ velvet { assembly_id = label }

parseLogFile :: Text -> VelvetInfo
parseLogFile t = case parse logFile t of
  Done _ r -> r
  _ -> error "bang"

