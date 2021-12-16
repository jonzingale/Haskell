module Downloader where
import Control.Monad (unless)
import System.FilePath (takeFileName)
import System.Posix (fileExist)
import System.Process (callProcess)

download url = do
    e <- fileExist $ takeFileName url
    -- unless e $ callProcess "curl" ["-O", url]
    unless e $ callProcess "curl" ["--insecure", url]
    return $ takeFileName url

main :: IO ()
main = do
    f <- download "https://redfish.com/pipermail/friam_redfish.com/2021-December/date.html"
    print f