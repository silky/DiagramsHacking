#!/usr/bin/env stack

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.Colour.Palette.BrewerSet
import Data.List (intersperse)
import Data.List hiding (union)
import Data.List.Split (divvy)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (union)
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Path.Boolean
import GSL.Random.Quasi
import System.Random

-- TODO: Make this less hacky
logoPoints = [ -- >
               [ (0, 3), (1.2, 1.5), (0,0) ]
               -- \
             , [ (0.8, 3), (0.8 + (2 * 1.2), 0) ]
               -- /
             , [ (0.8, 0), (0.8 + 1.2, 1.5) ]
               -- =
             , [ (2.2, 1.85), (4, 1.85) ]
             , [ (2.7, 1.32), (4, 1.32) ]
             ]


logo :: Diagram B
logo = Path trails # expandPath 0.2
                   # union Winding
                   # strokeP
                   # lw 0.1
                   # lc white
                   # centerXY
    where
        verts  = (map . map) p2 logoPoints
        trails = map fromVertices verts 


dropShadow :: Diagram B -> Diagram B
dropShadow d = d <> d # (translateX 0.4)
                      # (translateY (-0.4))
                      # fc black

-- addDropShadow d = d <> d # (translate (0.1, 0.2))


-- TODO: Use this.
wibble :: Deformation V2 V2 Double
wibble = Deformation $ \p ->
  ((p^._x) + 0.3 * cos ((p ^. _y) * tau)) ^& (p ^. _y)


bars = vcat' (with & sep .~ 0.5) (replicate 3 $ rect 10 0.3 # lw none)


getPoints :: QRNGType -> Int -> IO [Point V2 Double]
getPoints qrngType n = do
    rng     <- newQRNG qrngType 2
    points  <- replicateM n (getListSample rng)

    return $ map (\[a,b] -> mkP2 a b) points


transforms :: Diagram B -> IO (Diagram B)
transforms d = do
    s <- randomRIO (0.3, 1.2)
    r <- randomRIO (0,   360)
    c <- randomRIO (0, len)

    let colour = colours !! c

    return $ d # scale s
               # rotateBy r
               # fc colour
        where
            colours = brewerSet Set3 12
            len     = length colours - 1


retroHaskell :: IO (QDiagram B V2 Double Any)
retroHaskell = do
    -- scale
    let s = 0.01 / 1.8
        a = 55 * 2 + 5
        b = 15 * 2 + 5
        c = 1  * 2
        start = 101

    points    <- getPoints halton 1000

    lambdas   <- mapM (mkPoint (logo # dropShadow # scale s))
                      (take a $ drop start points)

    circles   <- mapM (mkPoint (circle 1 # lw none # dropShadow # scale s))
                      (take a $ drop (start + a) points)

    rects1    <- mapM (mkPoint (rect 8 1 # scale 0.7 # lw none # dropShadow # scale s))
                      (take b $ drop (start + 2*a) points)

    triangles <- mapM (mkPoint (triangle 3 # lw none # dropShadow # scale s))
                      (take b $ drop (start + b + 2*a) points)

    rects2    <- mapM (mkPoint (bars # dropShadow # scale s))
                      (take c $ drop (start + 2*b + 2*a) points)

    let diags = map position [ lambdas
                             , circles
                             , rects1
                             , triangles
                             , rects2
                             ]

    return $ mconcat diags # bg white
  where
      mkPoint d p = do
          d' <- transforms d
          return $ (p, d')


main = mainWith retroHaskell


