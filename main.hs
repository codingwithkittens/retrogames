{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- this will be something shortly
-- copied from getting started with netwire and sdl
import Prelude hiding ((.), id, null, filter)

import Control.Arrow
import Control.Wire
-- import Control.Monad (void)
import FRP.Netwire
import Data.Set as Set
--import Data.Monoid (Monoid)
import qualified Graphics.UI.SDL as SDL

newtype Xcoord = X Double deriving (Ord, Eq, Num, Real, Fractional, RealFrac)
newtype Ycoord = Y Double deriving (Ord, Eq, Num, Real, Fractional, RealFrac)
-- not sure if Vec should be a newtype
type Vec = (Xcoord, Ycoord)
x :: Vec -> Xcoord
x = fst
y :: Vec -> Ycoord
y = snd

{-
newtype Angle = A Double -- measured of course in radians
data PolarVector = PolarVector { theta::Angle, magnitude::Double }
-}

-- magic constants? aka game parameters
width = 400::Int
height = 600::Int
box_radius = 25 ::Int
coeff_friction = 0.8 :: Double -- Energy lost in collisions
gravity = -1::Double -- default acceleration downwards
-- also sprite information?

render :: SDL.Surface -> (Vec, Vec) -> IO ()
render screen (pos, vel) =
    do
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
        SDL.fillRect screen Nothing
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
        SDL.fillRect screen
            (Just $ SDL.Rect ((+(- box_radius)).round $ x pos) (((+)(- box_radius)).round $ y pos) (2*box_radius) (2*box_radius))
    SDL.flip screen

main :: IO ()
main =
  SDL.withInit [SDL.InitEverything] $ do
  screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
  moveblockwithinput screen clockSession_ updateState (0,0) Set.empty

 where
  moveblockwithinput screen sess wire vel keys = do
    keys'       <- parseEvents keys
    (dt, s')    <- stepSession sess
    (state, w') <- stepWire wire dt (Right (keys', vel))
    let ((xpos, ypos), (vx, vy)) = either (const ((0,0), (0,0))) id state
    -- should be able to use switch and edge rather than this hacky thing
    let vy' =
          if ((round ypos) < 0 || (round ypos) > height)
          then ((sign (2 * ypos < fromIntegral height)) * (abs vy))
          else vy
    let vx' =
          if ((round xpos) < 0 || (round xpos) > width)
          then ((sign (2 * xpos < fromIntegral height)) * (abs vx))
          else vx
    let v' = (vx', vy')
    -- let v' = (vx,vy)
    let x' = (xpos, ypos)
    render screen (x', v')
    moveblockwithinput screen s' w' v' keys'
    -- where are we keeping track of the position?

sign :: Num a => Bool -> a
sign False = -1
sign True = 1

updateState :: (HasTime t s, Monad m) => Wire s () m (Set SDL.Keysym,Vec) (Vec, Vec)
updateState =
    proc (keys, vel) -> do
         accel      <- acceleration -< keys
         (vx,vy)    <- arr (\((vx,vy), (vx',vy')) -> (vx + vx', vy + vy' - Y gravity)) -< (accel,vel)
         x <- integral 0 -< vx
         y <- integral 0 -< vy
         returnA -< ((x,y),(vx,vy)) 
         where acceleration = 
                       let keyDown k = 
                            not . null . filter ((==k) . SDL.symKey) 
                       in
                            pure (-2,  0) . when (keyDown SDL.SDLK_LEFT)
                        <|> pure ( 2,  0) . when (keyDown SDL.SDLK_RIGHT)
                        <|> pure ( 0, -2) . when (keyDown SDL.SDLK_UP)
                        <|> pure ( 0,  2) . when (keyDown SDL.SDLK_DOWN)
                        <|> pure ( 0,  0)

parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
    event <- SDL.pollEvent
    case event of
        SDL.NoEvent   -> return keysDown
        SDL.KeyDown k -> parseEvents (insert k keysDown)
        SDL.KeyUp k   -> parseEvents (delete k keysDown)
        _             -> parseEvents keysDown

deriving instance Ord SDL.Keysym
