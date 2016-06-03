{-# LANGUAGE OverloadedStrings #-}

module Html where

import Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A
import Prelude hiding (id)

page :: Html -> Html
page content_ = docTypeHtml $ do
   H.head $ H.title "Interference"
   body $ do
     h1 "Interference"
     content_

form :: Html
form = H.form $ do
  H.label ! for "sources" $ "Sources, as a list of tuples, ex. [(123,456,0), (432,11,23)] or [(115,225,0),(75,100,0),(15,150,0)]"
  br
  input ! type_ "text" ! id "sources"
  br
  H.label ! for "amplitude" $ "Amplitude (try 80)"
  br
  input ! type_ "number" ! id "amplitude"
  br
  H.label ! for "wavelength" $ "Wavelength (try 2)"
  br
  input ! type_ "number" ! id "wavelength"
  br
  H.label ! for "width" $ "Width (400 is reasonably fast)"
  br
  input ! type_ "number" ! id "width"
  br
  H.label ! for "height" $ "Height (400 is reasonably fast)"
  br
  input ! type_ "number" ! id "height"
  br
  button ! id "generate" $ "Generate (blocking)"
