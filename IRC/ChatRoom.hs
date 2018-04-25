module ChatRoom where
import Control.Monad (forever)
import Text.Printf
import System.IO
import Network

server = "irc.freenode.org"
port = 6667
chan = "#haskelldata"
nick = "awesome-dude-bot"

main = do
  h <- connectTo server (PortNumber (fromIntegral port))
  hSetBuffering h NoBuffering
  write h "Nick" nick
  write h "User" (nick ++ " 0 * :tutorial bot")
  write h "JOIN" chan
  listen h

write :: Handle -> String -> String -> IO()
write h s t = do
  hPrintf h "%s %s\r\n" s t
  printf "> %s %s\n" s t

listen :: Handle -> IO()
listen h = forever $ do
  s <- hGetLine h
  putStrLn s
