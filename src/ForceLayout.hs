{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE TypeFamilies              #-}

-- IDEA:
--
--  Replicate https://user-images.githubusercontent.com/129525/31580764-46fa5f06-b1a5-11e7-825d-0ea674606936.png
--  by using a physics model and gradient descent.

-- TODO:
--
--  - Include Tensorflow-Haskell
--  - Build some simple overlap minimisation equation
--  - Gradient descent
--  - ...
--  - Success!

import Diagrams.Backend.Cairo.CmdLine
import Diagrams.Prelude hiding (union)


forceLayout :: Diagram B
forceLayout = circle 1


main = mainWith forceLayout
