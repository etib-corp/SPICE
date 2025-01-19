module Files (secureGetContent, secureGetContentBS) where

import qualified Data.ByteString as BS
import System.IO.Error

getContent :: String -> IO String
getContent file = readFile file

catchString :: IOError -> IO String
catchString e = print e >> fail ""

catchBS :: IOError -> IO BS.ByteString
catchBS e = print e >> fail ""

secureGetContent :: String -> IO String
secureGetContent file = catchIOError (getContent file) catchString

secureGetContentBS :: String -> IO BS.ByteString
secureGetContentBS file = catchIOError (BS.readFile file) catchBS
