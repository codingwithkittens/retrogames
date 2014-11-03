{-# LANGUAGE Arrows #-}

-- this will be something shortly
-- copied from getting started with netwire and sdl
import Prelude hiding ((.), id, null, filter)

import Control.Arrow
import Control.Wire
import FRP.Netwire
import Data.Set as Set
-- import Debug.Trace
import qualified Graphics.UI.SDL as SDL
import qualified Graphics.UI.SDL.TTF as SDLTTF
import qualified Graphics.UI.SDL.Primitives as SDL

type Xcoord = Double
type Ycoord = Double
-- not sure if Vec should be a newtype
type Vec = (Xcoord, Ycoord)
x :: Vec -> Xcoord
x = fst
y :: Vec -> Ycoord
y = snd

data Polar = Polar { theta :: Double , radius :: Double }

polarToVec :: Polar -> Vec
polarToVec (Polar {theta = t, radius = r}) = -- (r * cos t, r * sin t)
    let
        x' = r * cos t
        y' = r * sin t
    in
    (x', y')

vecToPolar :: Vec -> Polar
vecToPolar (0, y') =
    let
        r = y'
        t = if y' > 0 then pi/2 else -pi/2
    in
    Polar {theta = t, radius = r}
vecToPolar (x', y') =
    let
        r = sqrt (x' * x' + y' * y')
        t = atan (y'/x') + (if x' > 0 then 0 else pi)
    in
    Polar {theta = t, radius = r}


-- magic constants? aka game parameters
width :: Int
width = 400
height :: Int
height = 600
boxRadius :: Int
boxRadius = 25
energyLoss :: Double -- Energy lost in collisions
energyLoss = 0.95
gravity :: Double -- default acceleration downwards
gravity = -1
-- also sprite information?

-- separating bearing from velocity vec
render :: SDL.Surface -> (Vec, Vec, Polar) -> SDLTTF.Font -> IO ()
render screen (pos, vel, bearing) font =
    do
    SDL.fillRect screen Nothing (SDL.Pixel 0xFFFFFF)
    -- Drawing velocity
    SDL.line screen (fromIntegral xcent) (fromIntegral ycent) 
        (fromIntegral $ xcent+ 10*dx) (fromIntegral $  ycent+ 10*dy) 
        (SDL.Pixel 0x00FFF0)
    -- Drawing box
    SDL.fillRect screen
            (Just $ SDL.Rect ( xcent - boxRadius) (ycent - boxRadius) (2*boxRadius) (2*boxRadius))
            (SDL.Pixel 0x0000FF)
    -- Drawing bearing
    SDL.line screen (fromIntegral xcent) (fromIntegral ycent) (fromIntegral $ xcent+ 10*bear_dx) (fromIntegral $  ycent+ 10*bear_dy)
        (SDL.Pixel 0x2020FF)
    renderString font 5 5 ("Current Pos:" ++ show xcent ++ ", " ++ show ycent)
    renderString font 5 50 ("Current Bearing:" ++ show (theta bearing) ++ ", " ++ show (radius bearing))
    SDL.flip screen
    where xcent = (width `div` 2) + round (x pos)
          ycent = (height `div` 2) + round (y pos)

          dx = round.(/4) $ x vel
          dy = round.(/4) $ y vel

          bear'   = polarToVec bearing
          bear_dx = round.(/2) $ x bear'
          bear_dy = round.(/2) $ y bear'

          produceString fnt str = SDLTTF.renderTextSolid fnt str (SDL.Color 0 0 0)
          renderString fnt xp yp str = produceString fnt str >>=
              (\text -> SDL.blitSurface text Nothing screen (Just $SDL.Rect xp yp (xp + 100) (yp+10)))

main :: IO ()
main =
    SDL.withInit [SDL.InitEverything] $ do
    SDLTTF.init
    font <- SDLTTF.openFont "DroidSans.ttf" 18
    screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
    moveblockwithinput screen font clockSession_ polarUpdateState (0,0) (Polar 0 0) Set.empty

 where
    moveblockwithinput screen font sess wire vel bear keys = do
    keys'       <- parseEvents keys
    (dt, s')    <- stepSession sess
    (state, w') <- stepWire wire dt (Right (keys', vel, bear))

    let (x', v', b') = either (const ((fromIntegral width/2,fromIntegral height/2), vel, bear)) id state
    render screen (x', v', b') font
    moveblockwithinput screen font s' w' v' b' keys'
    -- where are we keeping track of the position?

_updateState :: (HasTime t s, Monad m) => Wire s () m (Set SDL.SDLKey,Vec) (Vec, Vec)
_updateState =
    proc (keys, vel) -> do
         accel      <- acceleration -< keys
         (vx,vy)    <- arr (\((vx,vy), (vx',vy')) -> (vx + vx', vy + vy' - gravity)) -< (accel,vel)
         xx <- integral 0 -< vx
         yy <- integral 0 -< vy
         (x',vx') <- arr checkBounds -< (xx,vx,width `div` 2)
         (y',vy') <- arr checkBounds -< (yy,vy,height `div` 2)
         returnA -< ((x',y'),(vx',vy'))
         where acceleration =
                            pure (-2,  0) . when (member SDL.SDLK_LEFT)
                        <|> pure ( 2,  0) . when (member SDL.SDLK_RIGHT)
                        <|> pure ( 0, -2) . when (member SDL.SDLK_UP)
                        <|> pure ( 0,  2) . when (member SDL.SDLK_DOWN)
                        <|> pure ( 0,  0)
               checkBounds (pos,vel,bound) -- bounce mode
                      | posInt < -bound + boxRadius && vel < 0  = (fromIntegral (-bound + boxRadius),  -energyLoss * vel)
                      | posInt > bound - boxRadius  && vel > 0  = (fromIntegral (bound - boxRadius),  -energyLoss * vel)
                      | otherwise                                = (pos,vel)
                      where posInt = round pos

polarUpdateState :: (HasTime t s, Monad m) => Wire s () m (Set SDL.SDLKey,Vec,Polar) (Vec, Vec, Polar)
polarUpdateState =
    proc (keys, vel, bear) -> do
         accel      <- acceleration -< keys
         (vx,vy)    <- arr updateVelocity -< (accel,vel)
         bearing    <- arr updateBearing  -< (accel,bear)
         let vyg = vy - gravity
         xx <- integral 0 -< vx
         yy <- integral 0 -< vyg
         (x',vx') <- arr checkBounds -< (xx,vx,width `div` 2)
         (y',vy') <- arr checkBounds -< (yy,vyg,height `div` 2)
         returnA -< ((x',y'),(vx',vy'),bearing)
         where acceleration =
                            pure Polar {theta = -pi/600, radius = 0} . when (member SDL.SDLK_LEFT)
                        <|> pure Polar {theta =  pi/600, radius = 0} . when (member SDL.SDLK_RIGHT)
                        <|> pure Polar {theta =  0, radius =  0.5}   . when (member SDL.SDLK_UP)
                        <|> pure Polar {theta =  0, radius = -0.5}   . when (member SDL.SDLK_DOWN)
                        <|> pure Polar {theta =  0, radius =  0}
               checkBounds (pos,vel,bound) -- bounce mode
                      | posInt < -bound + boxRadius && vel < 0  = (fromIntegral (-bound + boxRadius), -energyLoss * vel)
                      | posInt > bound - boxRadius  && vel > 0  = (fromIntegral ( bound - boxRadius), -energyLoss * vel)
                      | otherwise                                = (pos,vel)
                      where posInt = round pos
               updateVelocity (Polar {theta = t, radius = r}, vec) =
                      let
                        Polar {theta = curt, radius = curr} = vecToPolar vec
                        newt = t + curt
                        newr = r + curr
                      in
                      polarToVec Polar {theta = newt ,radius = newr}
               updateBearing (Polar {theta = a_t, radius = _a_r}, Polar {theta = b_t, radius = _b_r}) 
                      = Polar {theta = a_t + b_t, radius = 50}

{-
_polarUpdateStateWithoutBearing :: (HasTime t s, Monad m) => Wire s () m (Set SDL.SDLKey,Vec) (Vec, Vec)
_polarUpdateStateWithoutBearing =
    proc (keys, vel) -> do
         accel      <- acceleration -< keys
         (vx,vy)    <- arr updateVelocity -< (accel,vel)
         let vyg = vy - gravity
         xx <- integral 0 -< vx
         yy <- integral 0 -< vyg
         (x',vx') <- arr checkBounds -< (xx,vx,width `div` 2)
         (y',vy') <- arr checkBounds -< (yy,vyg,height `div` 2)
         returnA -< ((x',y'),(vx',vy'))
         where acceleration =
                            pure Polar {theta = -pi/600, radius = 0} . when (member SDL.SDLK_LEFT)
                        <|> pure Polar {theta =  pi/600, radius = 0} . when (member SDL.SDLK_RIGHT)
                        <|> pure Polar {theta =  0, radius =  0.5}   . when (member SDL.SDLK_UP)
                        <|> pure Polar {theta =  0, radius = -0.5}   . when (member SDL.SDLK_DOWN)
                        <|> pure Polar {theta =  0, radius =  0}
               checkBounds (pos,vel,bound) -- bounce mode
                      | posInt < -bound + boxRadius && vel < 0  = (fromIntegral (-bound + boxRadius), -energyLoss * vel)
                      | posInt > bound - boxRadius  && vel > 0  = (fromIntegral ( bound - boxRadius), -energyLoss * vel)
                      | otherwise                                = (pos,vel)
                      where posInt = round pos
               updateVelocity (Polar {theta = t, radius = r}, vec) =
                      let
                        Polar {theta = curt, radius = curr} = vecToPolar vec
                        newt = t + curt
                        newr = r + curr
                      in
                      polarToVec Polar {theta = newt ,radius = newr}
-}


parseEvents :: Set SDL.SDLKey -> IO (Set SDL.SDLKey)
parseEvents keysDown = do
    event <- SDL.pollEvent
    case event of
        SDL.NoEvent   -> return keysDown
        SDL.KeyDown (SDL.Keysym k _ _) -> parseEvents (insert k keysDown)
        SDL.KeyUp (SDL.Keysym k _ _)   -> parseEvents (delete k keysDown)
        _             -> parseEvents keysDown

