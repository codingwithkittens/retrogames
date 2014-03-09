{-# LANGUAGE Arrows #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

-- this will be something shortly
-- copied from getting started with netwire and sdl
import Prelude hiding ((.), id, null, filter)

import Control.Arrow
import Control.Wire
import FRP.Netwire
import Data.Set as Set
import Debug.Trace
import qualified Graphics.UI.SDL as SDL


newtype Xcoord = X Double deriving (Show, Ord, Eq, Num, Real, Fractional, RealFrac)
newtype Ycoord = Y Double deriving (Show, Ord, Eq, Num, Real, Fractional, RealFrac)
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
    traceIO ("trying to draw" ++ show pos)
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
    -- traceIO ("state is " ++ show (Left state) ++ show (Right state))
    traceIO ("state is " ++  show state)

    let (x', v') = either (const ((X (fromIntegral width/2),Y (fromIntegral height/2)), vel)) id state
    -- let (x',v') = return state
    --    traceIO ("Current pos:" ++ show x' ++ "Current Vel:" ++ show v' )
    render screen (x', v')
    moveblockwithinput screen s' w' v' keys'
    -- where are we keeping track of the position?

updateState :: (HasTime t s, Monad m) => Wire s () m (Set SDL.Keysym,Vec) (Vec, Vec)
updateState =
    proc (keys, vel) -> do
         accel      <- acceleration -< keys
         --arr (\s -> traceIO ("vel0 is " ++  show s ++ "\n")) -< vel
         (vx,vy)    <- arr (\((vx,vy), (vx',vy')) -> (vx + vx', vy + vy' - Y gravity)) -< (accel,vel)
         xx <- integral 0 -< vx
         yy <- integral 0 -< vy
         arr (\s -> trace ("pos is " ++  show s )) -< (xx,yy)
         (x',vx') <- arr checkBounds -< (xx,vx,width)
         (y',vy') <- arr checkBounds -< (yy,vy,height)
         returnA -< trace ("oldpos is " ++  show (xx,yy) ) $trace ("newpos is " ++  show (x',y') ) $ ((x',y'),(vx',vy')) 
         where acceleration = 
                       let keyDown k = 
                            not . null . filter ((==k) . SDL.symKey) 
                       in
                            pure (-2,  0) . when (keyDown SDL.SDLK_LEFT)
                        <|> pure ( 2,  0) . when (keyDown SDL.SDLK_RIGHT)
                        <|> pure ( 0, -2) . when (keyDown SDL.SDLK_UP)
                        <|> pure ( 0,  2) . when (keyDown SDL.SDLK_DOWN)
                        <|> pure ( 0,  0)
               checkBounds (pos,vel,bound) 
                      | posInt < 0         = ((fromIntegral box_radius), -1 * vel)
                      | posInt > bound - box_radius = ((fromIntegral (bound - box_radius)), -1 * vel)
                      | otherwise                   = (pos,vel)
                      where posInt = (round pos)
                    

parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
    event <- SDL.pollEvent
    case event of
        SDL.NoEvent   -> return keysDown
        SDL.KeyDown k -> parseEvents (insert k keysDown)
        SDL.KeyUp k   -> parseEvents (delete k keysDown)
        _             -> parseEvents keysDown

deriving instance Ord SDL.Keysym
