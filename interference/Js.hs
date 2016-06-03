-- simple light/wave interaction generator - GHCJS version
{-# LANGUAGE ForeignFunctionInterface, JavaScriptFFI #-}
import Data.ByteString.Base64 (encode)
import qualified Gen as G
import GHCJS.DOM (webViewGetDomDocument, runWebGUI)
import GHCJS.DOM.Document (getBody)
import GHCJS.DOM.Element
import Data.Monoid
import Codec.Picture
import Data.ByteString.Char8 (unpack)
import Data.ByteString.Lazy (toStrict)
import Text.Blaze.Html.Renderer.String (renderHtml)
import GHCJS.Concurrent
import GHCJS.Foreign.Callback
import GHCJS.Types (JSString)
import GHCJS.DOM.Types (Window)
import qualified Data.JSString as JSS

import Html (page, form)

foreign import javascript unsafe
        "document.getElementById('amplitude').value" js_getAmplitude ::
        IO Float

foreign import javascript unsafe
        "document.getElementById('wavelength').value" js_getWavelength ::
        IO Float

foreign import javascript unsafe
        "document.getElementById('width').value" js_getWidth ::
        IO Float -- Int causes problems

foreign import javascript unsafe
        "document.getElementById('height').value" js_getHeight ::
        IO Float -- Int causes problems

foreign import javascript unsafe
        "document.getElementById('sources').value" js_getSources ::
        IO JSString

foreign import javascript interruptible
    "document.getElementById('generate').onclick = $1"
    js_set_onclick :: Callback a -> IO ()


main :: IO ()
main = runWebGUI $ \ webView -> do
  Just doc <- webViewGetDomDocument webView
  Just body <- getBody doc
  setInnerHTML body $ Just $ renderHtml $ page form
  callback <- syncCallback ContinueAsync $ Main.generateImage webView --TODO use async without realoding the page
  js_set_onclick callback

generateImage :: Window -> IO ()
generateImage webView = do
  a <- js_getAmplitude
  w <- js_getWavelength
  dim <- (,) <$> fmap round js_getWidth <*> fmap round js_getHeight
  ss <- (read . JSS.unpack) <$> js_getSources
  let i = encodeBitmap $ G.img dim ss a w
  Just doc <- webViewGetDomDocument webView
  Just body <- getBody doc
  let imgHtml = "<h1>Interference</h1><p>Generated:</p><img src=\"data:image/bmp;base64," <> unpack (encode $ toStrict i) <> "\">"
  setInnerHTML body $ Just imgHtml
   
