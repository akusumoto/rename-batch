{-# LANGUAGE DeriveDataTypeable #-}
module Lib where

import System.IO
import Data.List
import Data.List.Split
import Data.Data
import Data.Typeable
import System.Console.CmdArgs

data Language = English | Japanese deriving (Show, Data, Typeable, Eq)
data Option = Option {
    titlefile :: FilePath,
    language :: Language,
    files :: [FilePath]
} deriving (Show, Data, Typeable)

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
        item = (chapter, episode)
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
createRenameList' (Just file) title chapter episode language = [(file, newfile)]
    where
        ext = last $ splitOn "." file 
        newfile = createNewFileName title chapter episode ext language 

createRenameList _ _ [] _ = []
createRenameList files title (episode:episodes) language = result ++ next
    where
        chapter = fst episode
        target_file = findTargetFile files chapter
        result = createRenameList' target_file title chapter (snd episode) language
        next = createRenameList files title episodes language

showRenameList [] = return ()
showRenameList (l:ls) = do
    putStrLn $ (fst l) ++ " --> " ++ (snd l)
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
rename list = return ()

runRename :: String -> [(String, String)] -> IO ()
runRename answer list 
    | answer == "" || answer == "y" || answer == "Y" = rename list
    | otherwise = putStrLn "Aborted"

run opts = do
    list <- prepareRenameList (titlefile opts) (language opts) (files opts)
    
    putStr "Do you rename? [Y/n] " >> hFlush stdout
    ans <- getLine

    runRename ans list

    return ()
