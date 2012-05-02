-- Override generated paths for dev env:
-- http://neilmitchell.blogspot.se/2008/02/adding-data-files-using-cabal.html

module Paths_orgapp where

getDataFileName :: FilePath -> IO FilePath
getDataFileName = return
