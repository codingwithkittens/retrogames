-- this will be something shortly
-- copied from getting started with netwire and sdl
import Prelude hiding ((.), id)

import Control.Wire
import FRP.Netwire
import qualified Graphics.UI.SDL as SDL

main :: IO ()
main = SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode 200 200 32 [SDL.SWSurface]
  go screen clockSession challenge1

 where

  go screen session wire = do
    (x, wire', session') <- stepSession wire session ()

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
        SDL.fillRect screen Nothing

    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
        SDL.fillRect screen (Just $ SDL.Rect (round x) 0 50 50)

    SDL.flip screen
    go screen session' wire'

challenge1 :: Monad m => Wire e m Double Double
challenge1 = integral 0 . pure 20
