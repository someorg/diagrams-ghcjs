{-# LANGUAGE DeriveDataTypeable        #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE RankNTypes                #-}

module Tests
        ( Test(..)
        , examples
        ) where

import           Data.Typeable
import           Diagrams.Coordinates ((^&))
import           Diagrams.Core.Points
import           Diagrams.Prelude hiding (connect)
import           Diagrams.TwoD.Text

-----------------------------------------------------------------------

{-
data Test = Test
        String -- ^ the name of the test
        (forall canvas .
                ( Renderable (Path R2) canvas
                , Renderable Text      canvas
                , Backend canvas R2
                ) => Diagram canvas R2
        ) -- ^ and the diagram
-}

-----------------------------------------------------------------------

-- ^ list of cannonical examples.
examples =
        [
             square 1
        ]

poly_example = (poly1 ||| strutX 1 ||| poly2) # lwG 0.05
  where
          poly1 = polygon (with & polyType .~ PolyRegular 13 5
                           & polyOrient .~ OrientV)
          poly2 = polygon (with & polyType .~ PolyPolar  (repeat (1/40 @@ turn))
                           (take 40 $ cycle [2,7,4,6]))

data Corner = NW | NE | SW | SE
  deriving (Typeable, Eq, Ord, Show)
instance IsName Corner

connect n1 n2
  = withName n1 $ \b1 ->
    withName n2 $ \b2 ->
      atop ((location b1 ~~ location b2) # lc red # lwG 0.05)

squares =  (s # named NW ||| s # named NE)
       === (s # named SW ||| s # named SE)
  where s = square 1 # lwG 0.05

d = hcat' (with & sep .~ 0.5) (zipWith (|>) [0::Int ..] (replicate 5 squares))

pairs = [ ((0::Int) .> NE, (2::Int) .> SW)
        , ((1::Int) .> SE, (4::Int) .> NE)
        , ((3::Int) .> NW, (3::Int) .> SE)
        , ((0::Int) .> SE, (1::Int) .> NW)
        ]

connect_example = d # applyAll (map (uncurry connect) pairs)
