{-# LANGUAGE TypeApplications #-} 
module Main where
import Control.Exception (IOException, handle)
import Control.Monad (join, void, when)
import Data.Foldable (for_)
import Data.IORef (modifyIORef, newIORef, readIORef, writeIORef) 
import Data.List (isSuffixOf)
import System.Directory
  ( canonicalizePath
  , doesDirectoryExist 
  , doesFileExist
  , listDirectory
  )
import qualified Data.Set as Set (empty, insert, member) 
import Text.Printf (printf)

data FileType
  = FileTypeDirectory
  | FileTypeRegularFile 
  | FileTypeOther

classifyFile :: FilePath -> IO FileType 
classifyFile fname = do
  isDirectory <- doesDirectoryExist fname 
  isFile <- doesFileExist fname
  pure $ case (isDirectory, isFile) of
    (True, False) -> FileTypeDirectory 
    (False, True) -> FileTypeRegularFile 
    _otherwise -> FileTypeOther

dropSuffix :: String -> String -> String 
dropSuffix suffix s
  | suffix `isSuffixOf` s =
  take (length s - length suffix) s
  | otherwise = s

naiveTraversal :: FilePath -> (FilePath -> a) -> IO [a] 
naiveTraversal rootPath action = do
  classification <- classifyFile rootPath 
  case classification of
    FileTypeOther ->
      pure [] 
    FileTypeRegularFile ->
      pure $ [action rootPath] 
    FileTypeDirectory -> do
      contents <- map (fixPath rootPath) <$> listDirectory rootPath 
      concat <$> getPaths contents
  where
    fixPath parent fname = parent <> "/" <> fname
    getPaths = mapM (`naiveTraversal` action)

main :: IO ()
main = do
  putStrLn "Starting directory traversal..."

  putStrLn "Directory traversal completed."
