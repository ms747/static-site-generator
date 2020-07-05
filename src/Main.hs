{-# LANGUAGE OverloadedStrings #-}

module Main where

import CMark
import Data.Text(pack, unpack, replace)
import System.IO
import System.Directory
import System.Exit
import Control.Monad


main :: IO ()
main = generateSite

header :: FilePath
header = "source/header.html"

source :: FilePath
source = "source/pages/"

footer :: FilePath
footer = "source/footer.html"

destination :: FilePath
destination = "destination"

getFullPath :: FilePath -> IO [FilePath]
getFullPath path = ((path <>) <$> ) <$> listDirectory path

replaceHtmlWithMd :: String -> String
replaceHtmlWithMd = unpack . replace "md" "html" . pack

appendRootPath :: FilePath -> FilePath -> FilePath
appendRootPath src dest = src <> "/" <> dest


generateSite :: IO ()
generateSite = do
    header <- openFile header ReadMode
    footer <- openFile footer ReadMode
    headerContents <- hGetContents header
    footerContents <- hGetContents footer
    pagesPath <- listDirectory source
    pagesFullPath <- getFullPath source
    pagesHandle <- mapM (`openFile` ReadMode) pagesFullPath
    pagesContent <- mapM hGetContents pagesHandle
    let completePagesContent = map ((\x -> headerContents <> x <> footerContents) . convertMdtoHtml) pagesContent
    let pageWithData = zip (map ((destination `appendRootPath`) . replaceHtmlWithMd) pagesPath) completePagesContent
    doesDestinationExist <- doesDirectoryExist destination
    unless doesDestinationExist $ createDirectory destination
    mapM_ (uncurry writeFile) $ pageWithData


convertTextToNode :: String -> Node
convertTextToNode inp = commonmarkToNode [] $ pack inp

convertNodeToHtml :: Node -> String
convertNodeToHtml inp = unpack $ nodeToHtml [] inp

convertMdtoHtml :: String -> String
convertMdtoHtml = convertNodeToHtml . convertTextToNode
