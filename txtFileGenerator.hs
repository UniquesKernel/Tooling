#!/usr/bin/env stack
{- stack
  runghc
  --resolver lts-18.0 
  --install-ghc
  --package random
-}

import System.Directory
import System.FilePath
import Control.Monad
import GHC.Word
import Data.ByteString.Builder
import System.Random
import qualified Data.ByteString.Lazy as BS

-- Generate random content for a file
generateRandomContent :: IO BS.ByteString
generateRandomContent = do
    gen <- newStdGen
    let randomData = take 1024 $ randomRs ('a', 'z') gen  -- Generate 1 KB of random lowercase letters
    let randomData2 = take 3 $ randomRs ('c', 'c') gen  -- Generate 1 KB of random lowercase letters
    return $ BS.pack $ map (fromIntegral . fromEnum) (randomData2 ++ randomData)
 
generateFileNames :: IO String
generateFileNames = replicateM 10 (randomRIO ('a', 'z'))

generateFilePaths :: FilePath -> IO FilePath
generateFilePaths folder = do (folder </>) . (<> ".txt") <$> generateFileNames

main :: IO ()
main = do
    putStrLn "Enter the folder location where you want to create the folder:"
    folderLocation <- getLine

    putStrLn "Enter the name of the folder to be created:"
    folderName <- getLine

    let folderPath = folderLocation </> folderName

    createDirectoryIfMissing True folderPath
    putStrLn $ "Created folder: " ++ folderPath

    replicateM_ 200 $ do
        filePath <- generateFilePaths folderPath
        content <- generateRandomContent
        BS.writeFile filePath content
        putStrLn $ "Created file: " ++ filePath

