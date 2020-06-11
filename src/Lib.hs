{-# LANGUAGE DeriveDataTypeable #-}
module Lib where

import System.IO
import Data.List
import Data.List.Split
import Data.Data
import Data.Typeable
import System.Console.CmdArgs
import System.Directory
import Control.Monad

data Language = English | Japanese deriving (Show, Data, Typeable, Eq)
data Option = Option {
    titlefile :: FilePath,
    language :: Language,
    files :: [FilePath]
} deriving (Show, Data, Typeable)

data Episode = Episode {
    chapter :: String,
    episode :: String }

data RenameRule = RenameRule {
    old_file :: FilePath,
    new_file :: FilePath
}

option = Option {
        titlefile = "a.txt" &= help "Title list file (default: a.txt)",
        language = enum [Japanese &= help "Japanese mode (default)", English &= help "Enalish mode"],
        files = def &= args
    }
    &= summary "rename-batch v0.1"

{-
    contents
    --------------
    01 AAAAA
    02 BBBBB BBBB --------------
       v    
    lines contents = ["01 AAAAA","02 BBBBB BBBB"]
       v
    l = "02 BBBBB BBBB"
       v
    ws = ["02","BBBBB","BBBB"]
       v
    chapter = "02"
    episode = "BBBBB BBBB" 
       v
    item = ("02","BBBBB BBBB")
       v
    list = [("01","AAAAA"),("02","BBBBB BBBB")]
-}     
prepareEpisodeList' []     = []
prepareEpisodeList' (l:ls) = [item] ++ list
    where
        ws = words l
        chapter = head ws
        episode = unwords $ tail ws
        item = Episode {chapter = chapter, episode = episode}
        list = prepareEpisodeList' ls

prepareEpisodeList contents = prepareEpisodeList' $ lines contents

findTargetFile [] _ = Nothing
findTargetFile (file:files) chapter
    | is_target == True = Just file
    | otherwise         = findTargetFile files chapter
    where
        is_target = isInfixOf chapter file 

createNewFileName title chapter episode ext Japanese = title ++ " 第" ++ chapter ++ "話 「" ++ episode ++ "」." ++ ext
createNewFileName title chapter episode ext English  = title ++ " - " ++ chapter ++ " " ++ episode ++ "." ++ ext

createRenameList' Nothing _ _ _ _ = []
createRenameList' (Just file) title chapter episode language = [rename_rule]
    where
        ext = last $ splitOn "." file 
        newfile = createNewFileName title chapter episode ext language 
        rename_rule = RenameRule {old_file = file, new_file = newfile}

createRenameList _ _ [] _ = []
createRenameList files title (ep:eps) lang = result ++ next
    where
        ch = chapter ep
        target_file = findTargetFile files ch
        result = createRenameList' target_file title ch (episode ep) lang
        next = createRenameList files title eps lang

showRenameList [] = return ()
showRenameList (l:ls) = do
    let old = old_file l
    let new = new_file l
    putStrLn $ old ++ " --> " ++ new
    showRenameList ls

prepareRenameList file language files = do
    let sorted_files = sort files

    fh <- openFile file ReadMode 
    title <- hGetLine fh
    contents <- hGetContents fh

    let episode_list = prepareEpisodeList contents
    -- episode_list = [("01","AAAAA"),("02","BBBBB BBBB")]

    let rename_list = createRenameList sorted_files title episode_list language

    -- !! don't delete the following show operation to read to end of a file !!--
    showRenameList rename_list

    hClose fh

    return rename_list

{-
 - Run rename
 -}
rename [] = return ()
rename (l:ls) = do
    let old = old_file l
    let new = new_file l
    putStr $ old ++ " --> " ++ new ++ " ... "
    renameFile old new
    putStrLn "OK"
    rename ls

run opts = do
    list <- prepareRenameList (titlefile opts) (language opts) (files opts)
    if length list > 0
        then do putStr "Do you rename? [Y/n] " >> hFlush stdout
                ans <- getLine
                if ans == "" || ans == "y" || ans == "Y"
                    then do rename list
                    else do putStrLn "Aborted"
        else do return ()
    
