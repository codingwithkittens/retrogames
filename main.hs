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
-- import Debug.Trace
import qualified Graphics.UI.SDL as SDL
{-import qualified Graphics.UI.SDL.TTF as SDLTTF-}
{-import qualified Graphics.UI.SDL.Primitives as SDL-}

deriving instance Ord SDL.Keysym

newtype Xcoord = X Double deriving (Show, Ord, Eq, Num, Real, Fractional, RealFrac)
newtype Ycoord = Y Double deriving (Show, Ord, Eq, Num, Real, Fractional, RealFrac)
-- not sure if Vec should be a newtype
type Vec = (Xcoord, Ycoord)
x :: Vec -> Xcoord
x = fst
y :: Vec -> Ycoord
y = snd

data Polar = Polar { theta :: Double , radius :: Double }

polar_to_vec :: Polar -> Vec
polar_to_vec (Polar {theta = t, radius = r}) =
    let
        x' = r * cos t
        y' = r * sin t
    in
    (X x', Y y')

vec_to_polar :: Vec -> Polar
vec_to_polar (X 0, Y y') =
    let
        r = y'
        t = if y' > 0 then pi/2 else -pi/2
    in
    Polar {theta = t, radius = r}
vec_to_polar (X x', Y y') =
    let
        r = sqrt (x' * x' + y' * y')
        t = atan (y'/x') + (if x' > 0 then 0 else pi)
    in
    Polar {theta = t, radius = r}

{-
newtype Angle = A Double -- measured of course in radians
data PolarVector = PolarVector { theta::Angle, magnitude::Double }
-}

-- magic constants? aka game parameters
width :: Int
width = 400
height :: Int
height = 600
box_radius :: Int
box_radius = 25
coeff_friction :: Double -- Energy lost in collisions
coeff_friction = 0.8
gravity :: Double -- default acceleration downwards
gravity = -1
-- also sprite information?

render :: SDL.Surface -> (Vec, Vec) -> Int -> IO ()
{-render :: SDL.Surface -> (Vec, Vec) -> SDLTTF.Font -> IO ()-}
render screen (pos, vel) font =
    do
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 255 255 255 >>=
        SDL.fillRect screen Nothing
    (SDL.mapRGB . SDL.surfaceGetPixelFormat) screen 0 50 200 >>=
        SDL.fillRect screen
            (Just $ SDL.Rect ( xcent - box_radius) (ycent - box_radius) (2*box_radius) (2*box_radius))
    {-(SDL.mapRGB . SDL.surfaceGetPixelFormat) screen (fromIntegral 0) (fromIntegral 50) (fromIntegral 50) >>=-}
        {-SDL.line screen (fromIntegral xcent) (fromIntegral ycent) (fromIntegral $ xcent+dx) (fromIntegral $  ycent+dy)-}
    {-renderString font 5 5 ("Current Pos:" ++ show xcent ++ ", " ++ show ycent)-}
    SDL.flip screen
    where xcent = (width `div` 2) + (round $ x pos)
          ycent = (height `div` 2) + (round $ y pos)
          dx = round.(/4) $ x vel
          dy = round.(/4) $ y vel


          {-produceString fnt str = (SDLTTF.renderTextSolid fnt str (SDL.Color 0 0 0))-}
          {-renderString fnt xp yp str = produceString fnt str >>=-}
              {-(\text -> SDL.blitSurface text Nothing screen (Just $SDL.Rect xp yp (xp + 100) (yp+10)))-}

main :: IO ()
main =
    SDL.withInit [SDL.InitEverything] $ do
    {-SDLTTF.init-}
    {-font <- SDLTTF.openFont "DroidSans.ttf" 18-}
    screen <- SDL.setVideoMode width height 32 [SDL.SWSurface]
    {-moveblockwithinput screen font clockSession_ updateState (0,0) Set.empty-}
    moveblockwithinput screen 0 clockSession_ polarUpdateState (0,0) Set.empty

 where
    moveblockwithinput screen font sess wire vel keys = do
    keys'       <- parseEvents keys
    (dt, s')    <- stepSession sess
    (state, w') <- stepWire wire dt (Right (keys', vel))

    let (x', v') = either (const ((X (fromIntegral width/2),Y (fromIntegral height/2)), vel)) id state
    render screen (x', v') font
    moveblockwithinput screen font s' w' v' keys'
    -- where are we keeping track of the position?

updateState :: (HasTime t s, Monad m) => Wire s () m (Set SDL.Keysym,Vec) (Vec, Vec)
updateState =
    proc (keys, vel) -> do
         accel      <- acceleration -< keys
         (vx,vy)    <- arr (\((vx,vy), (vx',vy')) -> (vx + vx', vy + vy' - Y gravity)) -< (accel,vel)
         xx <- integral 0 -< vx
         yy <- integral 0 -< vy
         (x',vx') <- arr checkBounds -< (xx,vx,width)
         (y',vy') <- arr checkBounds -< (yy,vy,height)
         returnA -< ((x',y'),(vx',vy'))
         where acceleration =
                       let keyDown k =
                            not . null . filter ((==k) . SDL.symKey)
                       in
                            pure (-2,  0) . when (keyDown SDL.SDLK_LEFT)
                        <|> pure ( 2,  0) . when (keyDown SDL.SDLK_RIGHT)
                        <|> pure ( 0, -2) . when (keyDown SDL.SDLK_UP)
                        <|> pure ( 0,  2) . when (keyDown SDL.SDLK_DOWN)
                        <|> pure ( 0,  0)
               checkBounds (pos,vel,bound) -- bounce mode
                      | posInt < box_radius && vel < 0           = ((fromIntegral box_radius), -1 * vel)
                      | posInt > bound - box_radius  && vel > 0  = ((fromIntegral (bound - box_radius)), -1 * vel)
                      | otherwise                                = (pos,vel)
                      where posInt = (round pos)

polarUpdateState :: (HasTime t s, Monad m) => Wire s () m (Set SDL.Keysym,Vec) (Vec, Vec)
polarUpdateState =
    proc (keys, vel) -> do
         accel      <- acceleration -< keys
         (vx,vy)    <- arr updateVelocity -< (accel,vel)
         xx <- integral 0 -< vx
         yy <- integral 0 -< vy
         {-(x',vx') <- arr checkBounds -< (xx,vx,width)-}
         {-(y',vy') <- arr checkBounds -< (yy,vy,height)-}
         {-returnA -< ((x',y'),(vx',vy'))-}
         returnA -< ((xx,yy),(vx,vy))
         where acceleration =
                       let keyDown k =
                            not . null . filter ((==k) . SDL.symKey)
                       in
                            pure (Polar {theta = 0.5, radius = 0}) . when (keyDown SDL.SDLK_LEFT)
                        <|> pure (Polar {theta = -0.5, radius = 0}) . when (keyDown SDL.SDLK_RIGHT)
                        <|> pure (Polar {theta =  0, radius = 0.5}) . when (keyDown SDL.SDLK_UP)
                        <|> pure (Polar {theta =  0, radius = -0.5}) . when (keyDown SDL.SDLK_DOWN)
                        <|> pure (Polar {theta =  0, radius = 0})
               checkBounds (pos,vel,bound) -- bounce mode
                      | posInt < box_radius && vel < 0           = ((fromIntegral box_radius), -1 * vel)
                      | posInt > bound - box_radius  && vel > 0  = ((fromIntegral (bound - box_radius)), -1 * vel)
                      | otherwise                                = (pos,vel)
                      where posInt = (round pos)
               updateVelocity ((Polar {theta = t, radius = r}), vec) =
                      let
                        Polar {theta = curt, radius = curr} = vec_to_polar vec
                        newt = t + curt
                        newr = r + curr
                      in
                      polar_to_vec (Polar {theta = newt ,radius = newr})


parseEvents :: Set SDL.Keysym -> IO (Set SDL.Keysym)
parseEvents keysDown = do
    event <- SDL.pollEvent
    case event of
        SDL.NoEvent   -> return keysDown
        SDL.KeyDown k -> parseEvents (insert k keysDown)
        SDL.KeyUp k   -> parseEvents (delete k keysDown)
        _             -> parseEvents keysDown

