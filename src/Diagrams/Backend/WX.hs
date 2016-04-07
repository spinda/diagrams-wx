{-# LANGUAGE ForeignFunctionInterface #-}

-- | An optional add-on to the
-- <https://hackage.haskell.org/package/diagrams-cairo diagrams-cairo>
-- package which allows rendering diagrams in wxWidgets (using
-- <https://wiki.haskell.org/WxHaskell wxHaskell>).
--
-- wxHaskell doesn't support transparency when drawing 'Image's, so each
-- of these functions takes a 'W.Color' to use as the background color
-- when rendering the 'QDiagram'.
module Diagrams.Backend.WX (
    -- * Drawing to DC
    drawDiagram
    -- * Drawing to uImages
  , withDiagramImage
  , renderDiagramToNewImage
  , renderDiagramToImage
  , ImageSizeException(..)
  ) where

import Control.Exception
import Control.Monad

import Data.Bifunctor
import Data.Typeable

import Diagrams.Prelude
import Diagrams.Backend.Cairo
import Diagrams.Backend.Cairo.Internal

import Graphics.Rendering.Cairo

import Graphics.UI.WX as W hiding (bracket, when)
import Graphics.UI.WX.Draw

import Graphics.UI.WXCore.Image
import Graphics.UI.WXCore.WxcClassesAL
import Graphics.UI.WXCore.WxcClassTypes

import Foreign
import Foreign.C.Types

--------------------------------------------------------------------------------
-- Drawing to DC ---------------------------------------------------------------
--------------------------------------------------------------------------------

-- | Simple interface for drawing a diagram to a 'DC', like 'drawImage'.
--
-- Note that this creates, renders to, draws, and then deletes a
-- new 'Image' instance every invocation. Particularly performance-sensitive
-- users may want to look at using 'renderDiagramToImage' with the same 'Image'
-- instance every repaint.
drawDiagram :: (Monoid b, Semigroup b)
            => DC a
            -> QDiagram Cairo V2 Double b -> W.Point -> SizeSpec V2 Double
            -> W.Color -> [Prop (DC a)] -> IO ()
drawDiagram dc diagram point size bgColor props =
  withDiagramImage diagram size bgColor $ \image ->
    drawImage dc image point props

--------------------------------------------------------------------------------
-- Drawing to Images -----------------------------------------------------------
--------------------------------------------------------------------------------

-- | Safe wrapper around 'renderDiagramToNewImage' that ensures the 'Image'
-- instance is properly deleted when you're done using it.
--
-- > withDiagramImage diagram size =
-- >   bracket (renderDiagramToNewImage diagram size) imageDelete
withDiagramImage :: (Monoid b, Semigroup b)
                 => QDiagram Cairo V2 Double b -> SizeSpec V2 Double
                 -> W.Color -> (Image () -> IO c) -> IO c
withDiagramImage diagram size bgColor =
  bracket (renderDiagramToNewImage diagram size bgColor) imageDelete

-- | Create a new 'Image' of the appropriate size and render the given
-- 'QDiagram' to it with Cairo.
--
-- Make sure 'imageDelete' is called to free the 'Image' instance! Using
-- 'bracket' is recommended to ensure that the 'Image' is still freed in the
-- case of an exception:
--
-- > bracket (renderDiagramToNewImage diagram size) imageDelete doStuffWithImage
--
-- 'withDiagramImage' provides a convenient wrapper for this.
renderDiagramToNewImage :: (Monoid b, Semigroup b)
                        => QDiagram Cairo V2 Double b -> SizeSpec V2 Double
                        -> W.Color -> IO (Image ())
renderDiagramToNewImage diagram size bgColor = do
  let (w, h) = finalSize size diagram
  image <- imageCreateSized $ sz w h
  renderDiagramToImage' w h diagram bgColor image
  return image

-- | Render a 'QDiagram' to an existing 'Image'. Throws an 'ImageSizeException'
-- if the target 'Image' is too small for the resulting rendered diagram size.
renderDiagramToImage :: (Monoid b, Semigroup b)
                     => QDiagram Cairo V2 Double b -> SizeSpec V2 Double
                     -> W.Color -> Image i -> IO ()
renderDiagramToImage diagram size bgColor image = do
  let (w, h) = finalSize size diagram
  w' <- imageGetWidth image
  h' <- imageGetHeight image
  when (w' < w || w' < h) $ throw $ ImageSizeException (w, h) (w', h')
  renderDiagramToImage' w h diagram bgColor image

renderDiagramToImage' :: (Monoid b, Semigroup b)
                      => Int -> Int -> QDiagram Cairo V2 Double b
                      -> W.Color -> Image i -> IO ()
renderDiagramToImage' w h diagram bgColor image = do
  let fmt    = FormatARGB32
  let stride = formatStrideForWidth fmt w
  let size   = stride * h
  let opt    = CairoOptions
        { _cairoSizeSpec     = fromIntegral <$> dims2D w h
        , _cairoOutputType   = RenderOnly
        , _cairoBypassAdjust = False
        , _cairoFileName     = ""
        }
      (_, r) = renderDia Cairo opt diagram

  bracket (callocArray size) free $ \cairoPtr -> do
    withImageSurfaceForData cairoPtr fmt w h stride (`renderWith` r)
    withImageData image $ \wxPtr -> copyCairoToWxWidgets
      (colorRed bgColor) (colorGreen bgColor) (colorBlue bgColor)
      (fromIntegral size) cairoPtr (castPtr wxPtr)

finalSize :: (Monoid b, Semigroup b)
          => SizeSpec V2 Double -> QDiagram Cairo V2 Double b -> (Int, Int)
finalSize size = unr2 . fmap ceiling . fst . sizeAdjustment size . boundingBox

-- | Exception indicating that the target image was the wrong size (too small)
-- for the rendered diagram size.
data ImageSizeException
  -- | @ImageSizeException renderedDiagramSize targetImageSize@
  = ImageSizeException !(Int, Int) !(Int, Int)
  deriving (Show, Typeable)

instance Exception ImageSizeException where
  displayException (ImageSizeException needed avail) =
    "Can't render diagram with output size "
      ++ show needed
      ++ " to image of size "
      ++ show avail

foreign import ccall "util.c copyCairoToWxWidgets"
  copyCairoToWxWidgets :: CFloat -> CFloat -> CFloat
                       -> CPtrdiff -> Ptr CUChar -> Ptr CUChar
                       -> IO ()

