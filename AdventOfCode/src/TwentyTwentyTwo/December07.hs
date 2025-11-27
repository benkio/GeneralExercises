{-# LANGUAGE OverloadedStrings #-}

module TwentyTwentyTwo.December07 where

import Data.Bifunctor (bimap)
import Data.List (dropWhileEnd, find, isPrefixOf, sortOn)
import Data.Maybe (fromMaybe, listToMaybe, mapMaybe)
import Data.Text (pack, splitOn, unpack)
import Data.Tree (Tree (..), foldTree)
import Debug.Trace
import Text.Printf (printf)

data FileSystemNode = Dir String | File Int String deriving (Show)

data Command = CD String | LS deriving (Show)

data FileSystem = FileSystem {fs :: Tree FileSystemNode, pwd :: String} deriving (Show)

initialFileSystem :: FileSystem
initialFileSystem = FileSystem{fs = Node (Dir "/") [], pwd = ""}

getName :: FileSystemNode -> String
getName (Dir s) = s
getName (File _ s) = s

isFile :: FileSystemNode -> Bool
isFile (Dir _) = False
isFile (File _ _) = True

input :: IO String
input = readFile "input/2022/7December.txt"

executeCommand :: Command -> [String] -> FileSystem -> FileSystem
executeCommand (CD "/") _ (FileSystem{fs = fs}) = FileSystem{fs = fs, pwd = "/"}
executeCommand (CD "..") _ (FileSystem{fs = fs, pwd = path}) = FileSystem{fs = fs, pwd = ((\p -> if p == "/" then p else init p) . dropWhileEnd (/= '/')) path}
executeCommand (CD dir) _ (FileSystem{fs = fs, pwd = path}) =
    let pwd' = if last path == '/' then path ++ dir else path ++ "/" ++ dir
     in maybe (error (printf "path not found: %s - %s" pwd' (show fs))) (const (FileSystem{fs = fs, pwd = pwd'})) $ checkPath pwd' fs
executeCommand LS lsOutput fs =
    let children = fmap lsOutputToFs lsOutput
     in injectChildrens fs children

lsOutputToFs :: String -> Tree FileSystemNode
lsOutputToFs x
    | "dir " `isPrefixOf` x = Node (Dir (drop 4 x)) []
    | otherwise =
        let (size, filename) = (bimap (\y -> read y :: Int) tail . break (== ' ')) x
         in Node (File size filename) []

injectChildrens :: FileSystem -> [Tree FileSystemNode] -> FileSystem
injectChildrens (FileSystem{fs = fs, pwd = path}) childrens =
    FileSystem{fs = inject childrens dirs fs, pwd = path}
  where
    dirs = fromMaybe (error "path not found") $ checkPath path fs

inject :: [Tree FileSystemNode] -> [String] -> Tree FileSystemNode -> Tree FileSystemNode
inject [] _ fs = fs
inject _ [] fs = fs
inject _ _ f@(Node (File _ _) _) = f
inject cs (d : ds) f@(Node (Dir d') cs')
    | d == d' && null cs' && null ds = Node (Dir d') cs
    | d == d' && not (null cs') && null ds = error "impossible"
    | d == d' && null cs' && not (null ds) = error "impossible"
    | d == d' && not (null cs') && not (null ds) = Node (Dir d') $ fmap (inject cs ds) cs'
    | otherwise = f

checkPath :: String -> Tree FileSystemNode -> Maybe [String]
checkPath path fs =
    let paths = ("/" :) $ filter (not . null) $ unpack <$> splitOn "/" (pack path)
        check = selectPath paths fs
     in fmap (const paths) check

selectPath :: [String] -> Tree FileSystemNode -> Maybe (Tree FileSystemNode)
selectPath [] fs = Nothing
selectPath _ fs'@(Node (File _ _) _) = Nothing
selectPath (d : ds) fs'@(Node (Dir dir') children)
    | d == dir' && null ds = Just fs'
    | d == dir' && not (null ds) = listToMaybe $ mapMaybe (selectPath ds) children
    | otherwise = Nothing

parseInput :: [String] -> [(Command, [String])]
parseInput [] = []
parseInput ("$ cd /" : xs) = (CD "/", []) : parseInput xs
parseInput ("$ cd .." : xs) = (CD "..", []) : parseInput xs
parseInput ("$ ls" : xs) = (LS, takeWhile (not . ("$" `isPrefixOf`)) xs) : parseInput (dropWhile (not . ("$" `isPrefixOf`)) xs)
parseInput (x : xs)
    | "$ cd " `isPrefixOf` x = (CD (drop 5 x), []) : parseInput xs
    | otherwise = error $ printf "command %s not recognized" x

testInput :: String
testInput =
    "$ cd /\n\
    \$ ls\n\
    \dir a\n\
    \14848514 b.txt\n\
    \8504156 c.dat\n\
    \dir d\n\
    \$ cd a\n\
    \$ ls\n\
    \dir e\n\
    \29116 f\n\
    \2557 g\n\
    \62596 h.lst\n\
    \$ cd e\n\
    \$ ls\n\
    \584 i\n\
    \$ cd ..\n\
    \$ cd ..\n\
    \$ cd d\n\
    \$ ls\n\
    \4060174 j\n\
    \8033020 d.log\n\
    \5626152 d.ext\n\
    \7214296 k"

buildFileSystem :: String -> FileSystem
buildFileSystem = foldl (\fs (command, output) -> executeCommand command output fs) initialFileSystem . parseInput . lines

solution1 :: FileSystem -> Int
solution1 = sum . fmap snd . filter (\(d, s) -> s <= 100000 && (not . isFile) d) . fsToList . fs

fsToList :: Tree FileSystemNode -> [(FileSystemNode, Int)]
fsToList (Node (File s f) _) = [(File s f, s)]
fsToList (Node (Dir s) childrens) =
    let childSum = concatMap fsToList childrens
     in (Dir s, (sum . fmap snd . filter (isFile . fst)) childSum) : childSum

test x =
    let y = (fsToList . fs) x
     in (sum . fmap snd . filter (isFile . fst)) y == (snd . head) y

december07Solution1 :: IO Int
december07Solution1 = solution1 . buildFileSystem <$> input

solution2 :: FileSystem -> Int
solution2 fsys =
    let dirsNSpace = (sortOn snd . filter (\(d, s) -> (not . isFile) d) . fsToList . fs) fsys
        spaceToFree = 30000000 - (70000000 - (snd . last) dirsNSpace)
     in maybe (error "smallest dir not found?") snd $ find (\(_, s) -> s > spaceToFree) dirsNSpace

december07Solution2 :: IO Int
december07Solution2 = solution2 . buildFileSystem <$> input
