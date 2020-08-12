module VideoWriter where
import Codec.FFmpeg.Juicy

instance JuicyPixelFormat PixelYA8 where
  juicyPixelFormat _  = _ -- in memory format of PixelYA8

juicyToFFmpeg :: [Image PixelYA8] -> FilePath -> IO ()
juicyToFFmpeg is fp = do writer <- imageWriter params fp
                         -- give Just image data to writer to append it
                         forM_ is (writer . Just)
                         writer Nothing -- finalize, or else you'll break it
   where params :: EncodingParams
         params = _ -- Figure out what fps, width, height, etc. you want (hardcode? parameters to juicyToFFmpeg? fold on is?)