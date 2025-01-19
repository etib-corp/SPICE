module Files (secureGetContent) where

import System.IO.Error

getContent :: String -> IO String
getContent file = readFile file

catch :: IOError -> IO String
catch e = print e >> fail ""

secureGetContent :: String -> IO String
secureGetContent file = catchIOError (getContent file) catch
