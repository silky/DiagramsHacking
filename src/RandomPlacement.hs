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


main = undefined
