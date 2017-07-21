module Dalvik.Apk where

import Codec.Archive.Zip
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import Data.Conduit.List
import System.IO
import Path

import Dalvik.Parser
import Dalvik.Types

lazyToStrictBS :: LBS.ByteString -> BS.ByteString
lazyToStrictBS = BS.concat . LBS.toChunks

loadDexFromApkIO :: (Path Rel File) -> IO (Either String DexFile)
loadDexFromApkIO f = do
  entry <- parseRelFile "classes.dex" >>= mkEntrySelector
  chunks <- withArchive f (sourceEntry entry consume)
  -- TODO: this is silly. Should we tweak the parser to work with
  -- lazy ByteStrings?
  return . loadDex . BS.concat $ chunks

loadDexFromAnyIO :: FilePath -> IO (Either String DexFile)
loadDexFromAnyIO f = do
  h <- openFile f ReadMode
  c <- hGetChar h
  hClose h
  p <- parseRelFile f
  case c of
    'P' -> loadDexFromApkIO p
    'd' -> loadDexIO f
    _ -> return (Left "Invalid file format")
