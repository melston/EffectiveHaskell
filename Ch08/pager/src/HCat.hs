{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeApplications #-}
module HCat where

import qualified Control.Exception as Exception
import qualified Data.ByteString as BS
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import qualified Data.Time.Clock as Clock
import qualified Data.Time.Format as TimeFormat
import qualified System.Directory as Directory
import qualified System.Environment as Env
import System.IO
import qualified System.IO.Error as IOError
import qualified System.Info as SystemInfo
import System.Process (readProcess)
import Text.Printf (printf)

data ContinueCancel = Continue | Cancel deriving (Eq, Show)

data ScreenDimensions = ScreenDimensions
    {   screenRows :: Int
    ,   screenColumns :: Int
    } deriving (Show)

data FileInfo = FileInfo
    { filePath :: FilePath
    , fileSize :: Int
    , fileMTime :: Clock.UTCTime
    , fileReadable :: Bool
    , fileWriteable :: Bool
    , fileExecutable :: Bool
    } deriving Show

fileInfo :: FilePath -> IO FileInfo
fileInfo path = do
    perms <- Directory.getPermissions path
    mtime <- Directory.getModificationTime path
    size <- BS.length <$> BS.readFile path
    return FileInfo
        { filePath = path
        , fileSize = size
        , fileMTime = mtime
        , fileReadable = Directory.readable perms
        , fileWriteable = Directory.writable perms
        , fileExecutable = Directory.executable perms
        }

formatFileInfo :: FileInfo -> Int -> Int -> Int -> Text.Text
formatFileInfo FileInfo{..} 
               maxWidth 
               totalPages
               currentPage =
    let 
        permissionString = 
            [ if fileReadable then 'r' else '='
            , if fileWriteable then 'w' else '-'
            , if fileExecutable then 'x' else '-'
            ]
        timestamp = 
            TimeFormat.formatTime
            TimeFormat.defaultTimeLocale
            "%F %T"
            fileMTime
        statusLine = Text.pack $
            printf
            "%s | permissions: %s | %d bytes | modified: %s | page: %d of %d"
            filePath
            permissionString
            fileSize
            timestamp
            currentPage
            totalPages
    in invertText (truncateStatus statusLine)
    where
        invertText inputStr =
            let
                reverseVideo = "\^[[7m"
                resetVideo = "\^[[0m"
            in reverseVideo <> inputStr <> resetVideo
        truncateStatus statusLine
            | maxWidth <= 3 = ""
            | Text.length statusLine > maxWidth =
              Text.take (maxWidth - 3) statusLine <> "..."
            | otherwise = statusLine



handleArgs :: IO (Either String FilePath)
handleArgs =
    parseArgs <$> Env.getArgs
    where
      parseArgs argumentList =
        case argumentList of
          [fname] -> Right fname
          [] -> Left "No filename provided"
          _  -> Left "Multiple files not supported"

getContinue :: IO ContinueCancel
getContinue =
    hSetBuffering stdin NoBuffering
    >> hSetEcho stdin False
    >> hGetChar stdin
    >>= \case
        ' ' -> return Continue
        'q' -> return Cancel
        _ -> getContinue

showPages :: [Text.Text] -> IO ()
showPages [] = return ()
showPages (page:pages) =
    clearScreen
    >> TextIO.putStr page
    >> getContinue
    >>= \case
        Continue -> showPages pages
        Cancel -> return ()

clearScreen :: IO ()
clearScreen =
    BS.putStr "\^[[1J\^[[1;1H"

eitherToErr :: Show a => Either a b -> IO b
eitherToErr (Right a) = return a
eitherToErr (Left e) =
    Exception.throwIO . IOError.userError $ show e


groupsOf :: Int -> [a] -> [[a]]
groupsOf _ [] = []
groupsOf n elems =
    let (hd, tl) = splitAt n elems
    in hd : groupsOf n tl

wordWrap :: Int -> Text.Text -> [Text.Text]
wordWrap lineLength lineText
    | Text.length lineText <= lineLength = [lineText]
    | otherwise =
        let
            (candidate, nextLines) = Text.splitAt lineLength lineText
            (firstLine, overflow) = softWrap candidate (Text.length candidate - 1)
        in firstLine : wordWrap lineLength (overflow <> nextLines)
    where
        softWrap hardwrappedText textIndex
            | textIndex <= 0 = (hardwrappedText,Text.empty)
            | Text.index hardwrappedText textIndex == ' ' =
                let (wrappedLine, rest) = Text.splitAt textIndex hardwrappedText
                in (wrappedLine, Text.tail rest)
            | otherwise = softWrap hardwrappedText (textIndex - 1)

paginate :: ScreenDimensions -> FileInfo -> Text.Text -> [Text.Text]
paginate (ScreenDimensions rows cols) finfo text =
    let 
        rows' = rows - 1
        wrappedLines = concatMap (wordWrap cols) (Text.lines text)
        pages = map (Text.unlines . padTo rows') $ groupsOf rows' wrappedLines
        pageCount = length pages
        statusLines = map (formatFileInfo finfo cols pageCount) [1..pageCount]
    in zipWith (<>) pages statusLines
    where
        padTo :: Int -> [Text.Text] -> [Text.Text]
        padTo lineCount rowsToPad =
            take lineCount $ rowsToPad <> repeat ""

getTerminalSize :: IO ScreenDimensions
getTerminalSize =
    case SystemInfo.os of
        "darwin" -> tputScreenDimensions
        "linux" -> tputScreenDimensions
        _other -> pure $ ScreenDimensions 25 80
    where
        tputScreenDimensions :: IO ScreenDimensions
        tputScreenDimensions =
            readProcess "tput" ["lines"] ""
            >>= \tlines ->
                readProcess "tput" ["cols"] ""
                >>= \cols ->
                    let lines' = read $ init tlines
                        cols' = read $ init cols
                    in return $ ScreenDimensions lines' cols'

runHCat :: IO ()
-- runHCat = return ()2
runHCat = 
    handleIOError $ do
        targetFilePath <- eitherToErr =<< handleArgs
        -- contents <- TextIO.hGetContents =<< openFile targetFilePath ReadMode
        -- alternatively use a nested do block:
        contents <- do
            handle <- openFile targetFilePath ReadMode
            TextIO.hGetContents handle
        termSize <- getTerminalSize
        hSetBuffering stdout NoBuffering
        finfo <- fileInfo targetFilePath 
        let pages = paginate termSize finfo contents
        showPages pages
    where
        handleIOError :: IO () -> IO ()
        handleIOError ioAction =
            Exception.catch ioAction $
            \e -> putStrLn "Error: " >> print @IOError e


