module Input ( Parser, parseFile, parseFile' ) where

import Control.Monad ((<=<))
import Data.Bifunctor (first)
import Data.Text (Text)
import Data.Void (Void)
import Text.Megaparsec

import qualified Data.ByteString    as BS (readFile)
import qualified Data.Text.Encoding as E  (decodeUtf8')

type Parser = Parsec Void Text

{-|
  Take a Megaparsec parser and a path to a file, read the file,
  decode it assuming utf8, and apply the parser.
  If all this succeeds, the parser result is found in Right.
  If some of the steps fail, the error message is found in Left as String.
  Since we are reading files, all of this is wrapped inside the IO monad
-}
parseFile :: Parser a
          -> FilePath
          -> IO (Either String a) 
parseFile p f = (parseShowError <=< decodeShowError) <$> BS.readFile f
  where
    parseShowError = first errorBundlePretty . parse p f
    decodeShowError = first show . E.decodeUtf8'

{-|
  The unsafe variant of parseFile.
  If the result of parsing is Left, throw error.
  Otherwise, unwrap the result.
-}
parseFile' :: Parser a
           -> FilePath
           -> IO a
parseFile' p f = either error id <$> parseFile p f
