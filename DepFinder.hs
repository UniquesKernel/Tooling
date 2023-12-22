#!/usr/bin/env stack 
-- stack script --resolver lts-18.0 

import System.Process
import Text.Regex.TDFA ((=~))
import Data.List (isInfixOf)
import Control.Monad (mapM_)

-- GLOBAL VARIABLES

projectRoot :: String
projectRoot = "/home/mokri/git/cloud/root"

-- HELPER FUNCTIONS

extractKey :: String -> Maybe (String, String)
extractKey str =
  let pattern = "Pair\\(\"([^\"]+)\", \"([^\"]+)\".*" :: String
      (_, _, _, matches) = str =~ pattern :: (String, String, String, [String])
  in case matches of
       [name1, name2] -> Just (name1, name2)
       _ -> Nothing

sanitiseInputFromFile :: String -> String 
sanitiseInputFromFile = clean . encapsulate
  where 
    clean = filter (/= '\r') . filter (/= '\n') . filter (/= '\t')
    encapsulate str' = "'" <> str' <> "'"

-- GREP AND FIND DEPENDENCIES IN BUILD FILES

grepProcess' :: String -> String -> IO String 
grepProcess' dep path = do readCreateProcess (shell ("grep -rinH " ++ dep ++ " " <> "'" <> path <> "'")) ""

grepProcess :: String -> FilePath -> IO String 
grepProcess dependency filePath = do
    let findCommand = "find " ++ filePath ++ " -type f \\( -name \"build.gradle.kts\" -o -name \"libraries.gradle.kts\" \\)"
    let grepCommand = "grep -rinH " ++ dependency
    let command = findCommand ++ " -exec " ++ grepCommand ++ " {} +"
    readCreateProcess (shell command) ""

-- EXTRACT DEPENDENCIES FROM FILES AND PRINT

extractDependenciesFromFile :: FilePath -> IO [String]
extractDependenciesFromFile filePath = do 
  content <- readFile filePath
  (return . lines) content


printDependenciesFromFile :: String -> IO ()
printDependenciesFromFile dep = do 
  let sanitisedDependency = sanitiseInputFromFile dep
  
  grepResult <- grepProcess sanitisedDependency projectRoot

  putStrLn $ "\nDependencies for " ++ sanitisedDependency ++ " not using Pair(key, value) and libraries.gradle.kts"
  (mapM_ putStrLn . lines) grepResult
  
  case extractKey grepResult of 
    Just (name1, name2) -> do 
      putStrLn $ "\nDependencies for " ++ sanitisedDependency ++ " using Pair(key, value)\n"
      newGrep <- grepProcess ("'" <> name1 <> "'") projectRoot
      mapM_ print $ lines newGrep
    Nothing -> putStrLn "\nFound Nothing - Perhaps the target key either does not exist or is spread across multiple lines" 

main :: IO ()
main = do 
  let root = projectRoot
  let home = "/home/mokri/"
  
  -- In the future this should be made more generic by taking the file as input from
  -- user
  contentL <- extractDependenciesFromFile (home <> "Experimentation/test.txt")
  mapM_ printDependenciesFromFile (drop 2 contentL)

{- This is left here for reference to integrate both file reading and single
 - dependency search at the behest of the user 
 
  putStr "Please provide the dependency you are looking for: "
  dep <- getLine
  grepResult <- grepProcess ("'" <> dep <> "'") root

  putStrLn "\nDependencies not using Pair(key, value) and libraries.gradle.kts"
  (mapM_ putStrLn . lines) grepResult
  
  case extractKey grepResult of 
    Just (name1, name2) -> do 
      putStrLn "\nDependencies using Pair(key, value)"
      newGrep <- grepProcess ("'" <> name1 <> "'") root
      mapM_ print $ lines newGrep
    Nothing -> putStrLn "Found Nothing - Perhaps the target either does not exist or is spread across multiple lines" 
-}
