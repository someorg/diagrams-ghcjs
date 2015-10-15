{-# LANGUAGE OverloadedStrings, ViewPatterns, GADTs , NoMonomorphismRestriction#-}
module Main where

import Data.Monoid ((<>))
import Diagrams.Prelude hiding ((<>))
import Diagrams.Backend.GHCJS
import Control.Monad
import Control.Monad.Trans
import JavaScript.JQuery
import JavaScript.Web.Canvas (Context, getContext, Canvas)
import GHCJS.Foreign
import GHCJS.Types
import qualified Data.Text as T
import qualified Graphics.Rendering.GHCJS as G
import Diagrams.Backend.GHCJS
import qualified JavaScript.Web.Canvas as C
import qualified JavaScript.Web.Canvas.Internal as CI
import GHCJS.Marshal (ToJSVal(..))
import GHCJS.Marshal.Pure 
import JavaScript.Array ((!))



import Diagrams.TwoD as D

import Debug.Trace

foreign import javascript unsafe "$1.get(0).getContext('2d')"
  my_getContext :: JQuery -> IO C.Context

foreign import javascript unsafe "console.log($1);"
  f :: C.Context -> IO ()

mkContext nm = do
    testarea <- select "#main"
    let img = "<img style=\"border:1px solid #d3d3d3;\" "
              <> "src=\"../ref/" <> nm <> ".png\" />"
    let canvas = "<canvas id=\"" <> nm <> "\" width=\"200\" height=\"200\""
                 <> "style=\"border:1px solid #d3d3d3;\">"
                 <> "</canvas><br />"
    append ("<tr>" <> "<td valign=\"top\">" <> canvas <> "</td>" <> "</tr>") testarea

    my_getContext =<<  select ("#" <> nm)

sizepec = D.dims2D 200 200

main = do
    body <- select "#main"
    ctx <- mkContext ("a")
    f ctx
    renderDia Canvas (CanvasOptions sizepec ctx) (circle 1 #fc blue :: Diagram Diagrams.Backend.GHCJS.Canvas)
    putStrLn "end"

