module Camera where

import           Ray
import           Vec3
import           Data.Vec3
import           System.Random

type Scale = Integer
type Aspect = (Integer, Integer)
horizontalAspect :: Aspect -> Integer
horizontalAspect (h, _) = h
verticalAspect :: Aspect -> Integer
verticalAspect (_, v) = v
aspectRatio :: Aspect -> Double
aspectRatio (h, w) = fromInteger h / fromInteger w

data CameraType = Orthogonal | Perspective -- define a CameraType data type with two possible values: Orthogonal or Perspective

data Camera = Camera { aspect :: Aspect -- aspect ratio of the image
                     , scale :: Scale -- scale factor of the image
                     , position :: CVec3 -- position of the camera
                     , horizontal :: CVec3 -- horizontal vector of the image plane
                     , vertical :: CVec3 -- vertical vector of the image plane
                     , lowerLeftCorner :: CVec3 -- lower left corner of the image plane
                     , lensRadius :: Double -- radius of the lens
                     , uVector :: CVec3 -- u (horizontal) vector of the camera
                     , vVector :: CVec3 -- v (vertical) vector of the camera
                     , wVector :: CVec3 -- w (forward) vector of the camera
                     -- this field is a function that generates a ray given a camera and a UV coordinate
                     , getRay :: Camera -> UV -> StdGen -> (Ray, StdGen)
                     }

getCamera
  :: CVec3
  -> CVec3
  -> CVec3
  -> Aspect
  -> Scale
  -> Double
  -> Double
  -> Double
  -> CameraType
  -> Camera
getCamera pos lookAt vUp aspect scale fov aperture focusDistance cameraType =
  Camera
    { aspect          = aspect
    , scale           = scale
    , position        = pos
    , horizontal      = u .^ (2 * focusDistance * halfWidth)
    , vertical        = v .^ (2 * focusDistance * halfHeight)
    , lowerLeftCorner = pos
                        <-> u
                        .^  (halfWidth * focusDistance)
                        <-> v
                        .^  (halfHeight * focusDistance)
                        <-> w
                        .^  focusDistance
    , lensRadius      = aperture / 2
    , uVector         = u
    , vVector         = v
    , wVector         = w
    , getRay          = case cameraType of
                          Orthogonal  -> getOrthogonalRay
                          Perspective -> getPerspectiveRay
    }
 where
  fovTheta   = fov * pi / 180
  halfHeight = tan $ fovTheta / 2
  halfWidth  = aspectRatio aspect * halfHeight
  u          = normalize $ vUp >< w
  v          = w >< u
  w          = normalize $ pos <-> lookAt

type UV = (Double, Double)

-- | compute the number of horizontal pixels in the image by multiplying
-- the scale of the camera by the horizontal aspect ratio
nPixelsHorizontal camera = scale camera * horizontalAspect (aspect camera)
-- | compute the number of vertical pixels in the image by multiplying the
-- scale of the camera by the vertical aspect ratio
nPixelsVertical camera = scale camera * verticalAspect (aspect camera)

-- | convert an xy coordinate into an UV coordinate
toUV camera (x, y) = (u, v) :: UV
 where
  -- compute the u coordinate by dividing the x coordinate by the number of horizontal pixels
  u = x / fromInteger (nPixelsHorizontal camera)
  -- compute the v coordinate by dividing the y coordinate by the number of vertical pixels
  v = y / fromInteger (nPixelsVertical camera)

-- | getPerspectiveRay generates a perspective ray given a camera and a UV coordinate.
getPerspectiveRay :: Camera -> UV -> StdGen -> (Ray, StdGen)
getPerspectiveRay cam (u, v) rng =
  ( Ray
    -- compute the ray's origin by adding the camera's position and the random offset
    { Ray.origin = position cam <+> offset
    -- compute the ray's direction by adding the lower left corner of the image plane,
    -- the horizontal and vertical vectors scaled by u and v, and subtracting the camera
    -- position and the random offset
    , direction  = lowerLeftCorner cam
                   <+> (horizontal cam .^ u)
                   <+> (vertical cam .^ v)
                   <-> position cam
                   <-> offset
    }
  , nextRng -- return the ray and the new random number generator state
  )
 where
  -- generate a random point in a disk and return the point and the updated random number generator state
  (randomPoint, nextRng) = randomPointInDisk rng
  -- compute the x and y offsets by scaling the random point by the lens radius and converting it to cartesian coordinates
  (offX, offY, _)        = toXYZ $ randomPoint .^ lensRadius cam
  -- compute the offset by scaling the u and v vectors by the x and y offsets
  offset                 = uVector cam .^ offX <+> vVector cam .^ offY

-- | getOrthogonalRay generates an orthogonal ray given a camera and a UV coordinate.
getOrthogonalRay :: Camera -> UV -> StdGen -> (Ray, StdGen)
getOrthogonalRay cam (u, v) rng =
  ( Ray
    { Ray.origin = invert
                   $   lowerLeftCorner cam -- compute the origin of the ray by subtracting the lower left corner of the image plane from the origin
                   <+> (horizontal cam .^ (1 - u)) -- add the scaled horizontal vector
                   <+> (vertical cam .^ (1 - v)) -- add the scaled vertical vector
    , direction  = invert $ wVector cam -- compute the direction of the ray by inverting the w vector
    }
  , rng -- return the ray and the unchanged random number generator state
  )

