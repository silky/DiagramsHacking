{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

import Control.Monad
import GSL.Random.Quasi
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude
import Data.List (intersperse)
import System.Random (randomRIO)
import Data.List.Split (divvy)

main :: IO ()
-- Example 1 - Random points
-- main = mainWith stdRandomSample
--
-- Example 2 - Grids!
-- main = mainWith gridPlacement1
--
-- Example 2.2 - Sub-Grids!
-- main = mainWith gridPlacement2
--
-- Example 3 - Halton!
-- main = mainWith (samples halton)
--
-- Example 4 - Sobol!
main = mainWith (samples sobol)


getPoints :: QRNGType -> Int -> IO [Point V2 Double]
getPoints qrngType n = do
    rng     <- newQRNG qrngType 2
    points  <- replicateM n (getListSample rng)

    return $ map (\[a,b] -> mkP2 a b) points


mkPoint :: P2 Double -> (P2 Double, Diagram B)
mkPoint p = (p, circle 0.1 # lw none # scale 0.2)


samples :: QRNGType -> IO (Diagram B)
samples t = do
    let n = 100

    points <- getPoints t n
    let diag = position (map mkPoint points) # fc magenta
                                             # centerXY
    
    return $ diag # bg white


-- Quasi-Sampling

someSamples :: IO ([Diagram B])
someSamples = do
    let n = 150

    points <- mapM (flip getPoints n) [ halton
                                      , sobol
                                      , reverseHalton
                                      ]

    let diags = zipWith (\ps c -> (position (map mkPoint ps) # fc c))
                        points
                        (cycle [magenta, blue, orange])

    return diags


separate :: IO (Diagram B)
separate = do
    samples <- someSamples
    return $ (foldl1 (|||) (intersperse (strutX 0.2) samples))
                # centerXY


overlaid :: IO (Diagram B)
overlaid = do
    samples <- someSamples
    return $ mconcat samples
                # centerXY
                # fc red


allTogether :: IO (Diagram B)
allTogether = do
    diags <- sequence [separate
                      , overlaid
                      , stdRandomSample
                      ]

    return $ vcat (intersperse (strutY 0.2) diags)


gridPlacement1 :: IO (Diagram B)
gridPlacement1 = do
    let n = 100

    points <- replicateM n (mkP2 <$> rs <*> rs)

    let diags   = map (\p -> circle 1.2 # fc magenta # moveTo p # lw none <> rect 10 10 # lw none) points
        gridded = divvy 10 10 diags
        diag    = vcat (map hcat gridded)

    return $ diag # centerXY
                  # bg white
   where
    rs = randomRIO (-5, 5)


gridPlacement2 :: IO (Diagram B)
gridPlacement2 = do
    let n = 100

    points <- replicateM n (mkP2 <$> rs <*> rs)

    let subPoints = divvy 5 5 points

    let diags   = map (map (\p -> circle 1.2 # fc magenta # moveTo p # lw none <> rect 10 10 # lw none)) subPoints
        gridded = divvy 10 10 (join diags)
        diag    = vcat (map hcat gridded)

    return $ diag # centerXY
                  # bg white
   where
    rs = randomRIO (-5, 5)


stdRandomSample :: IO (Diagram B)
stdRandomSample = do
    let n = 100
    points <- replicateM n (mkP2 <$> rs <*> rs)
    return $ position (map mkPoint points)
                # fc magenta
                # centerXY
   where
    rs = randomRIO (0, 1)


