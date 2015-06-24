
module Velvet.Scraper (vencode) where

import Data.ByteString.Lazy.Char8 (ByteString)
import qualified Data.Text.IO as Text
import Data.Csv (encodeByName)
import Data.Traversable (traverse)
import Velvet.Scraper.Parser (parseLogFile, VelvetInfo, viHeader)

parseAssemblies :: [FilePath] -> IO [VelvetInfo]
parseAssemblies = fmap (fmap parseLogFile) . traverse Text.readFile

encodeVelvetInfo :: [VelvetInfo] -> ByteString
encodeVelvetInfo = encodeByName viHeader

vencode :: [FilePath] -> IO ByteString
vencode = fmap encodeVelvetInfo . parseAssemblies

