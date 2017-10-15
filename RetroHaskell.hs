#!/usr/bin/env stack
-- stack script --resolver lts-9.0 --package diagrams --package diagrams-lib --package diagrams-contrib --package random --package split --package palette --package diagrams-cairo --package mwc-random

{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Control.Monad (replicateM)
import Control.Monad.IO.Class (liftIO)
import Data.List.Split (divvy)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (union)
import Diagrams.TwoD.Offset
import Diagrams.TwoD.Path.Boolean
import System.Random
import System.Random.MWC (createSystemRandom, uniformR, GenIO)
import Data.Colour.Palette.BrewerSet
import Data.List hiding (union)


-- TODO:
--
--  - Make the mkGrid function nicer
--  - Figure out how sizes work relative to the input size
--  - Something


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
                   # centerXY
    where
        verts  = (map . map) p2 logoPoints
        trails = map fromVertices verts 


randR :: (Double, Double) -> GenIO -> IO Double
randR (a,b) gen = do
    uniformR (a, b) gen


randColours :: IO [Colour Double]
randColours = do
    sg <- getStdGen
    return $ map (colours !!) $ randomRs (0, len) sg
    where
        len     = length colours - 1
        colours = brewerSet Set3 12


mkGrid :: (Int, Int) 
       -> (Double, Double) 
       -> GenIO 
       -> Diagram B 
       -> IO (QDiagram B V2 Double Any)
mkGrid (gw, gh) (rw, rh) gen obj = do

    let total = gw * gh
        mm    = rw - 5.4
        bds   = (-mm, mm)

    points    <- replicateM total (mkP2 <$> randR bds gen <*> randR bds gen)
    rotations <- replicateM total (randomRIO (0, 360))
    scales    <- replicateM total (randomRIO (0.3, 1))
    colours   <- randColours

    let lambdas = zipWith4 mkLambda 
                            points 
                            rotations 
                            scales 
                            colours

        mkLambda p r s c = obj  # scale s
                                # moveTo p
                                # fc c
                                # rotateBy r
                                <> rect rw rh
                                    # lcA transparent

    let gridded    = divvy gw gw lambdas
        joined     = vcat (map hcat gridded)

    return $ joined # centerXY


-- TODO: Use this.
wibble :: Deformation V2 V2 Double
wibble = Deformation $ \p ->
  ((p^._x) + 0.3 * cos ((p ^. _y) * tau)) ^& (p ^. _y)


bars = vcat' (with & sep .~ 0.5) (replicate 3 $ rect 10 0.3 # lw 0.1)


retroHaskell :: IO (QDiagram B V2 Double Any)
retroHaskell = do
    gen   <- createSystemRandom

    lambdas   <- replicateM 2 (mkGrid (6,6) (10, 10) gen logo )
    triangles <- replicateM 1 (mkGrid (3,3) (20, 20) gen (triangle 4  # lw 0.1))
    circles   <- replicateM 2 (mkGrid (3,3) (20, 20) gen (circle 1.5  # lw 0.1))
    rect1s    <- replicateM 1 (mkGrid (3,3) (20, 20) gen (rect 10 1   # lw 0.1))
    rect2s    <- replicateM 1 (mkGrid (1,1) (60, 60) gen (bars))

    let stuff = [ lambdas, triangles, circles, rect1s, rect2s ]

    let final = mconcat (map mconcat stuff)
                    # bg black
                    # clipTo (rect 600 600)

    return final


main = mainWith retroHaskell


