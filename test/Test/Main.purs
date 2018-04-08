module Test.Main where

import DOM.JSDOM
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import DOM.Node.Document (documentURI)
import DOM.Node.Node (textContent, firstChild)
import DOM.Node.Types (Node, documentToNode)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Prelude (Unit, bind, discard, pure, unit, ($), (<#>), (==), (>>=))
import Test.Assert (ASSERT, assert)

firstText :: forall eff. Node -> Eff (dom :: DOM | eff) (Maybe String)
firstText node =
    firstChild node >>= traverse textContent

exampleHTML :: String
exampleHTML = "<p>hogeika</p>"

exampleURI :: String
exampleURI = "http://www.nicovideo.jp/"

main :: Eff (console :: CONSOLE, jsdom :: JSDOM, dom :: DOM, assert :: ASSERT, exception :: EXCEPTION) Unit
main = do
  log "Testing jsdom"
  text <- (jsdom exampleHTML {}) <#> documentToNode >>= firstText
  assert $ text == Just "hogeika"

  log "Testing jsdom config"
  uri <- (jsdom exampleHTML { url: exampleURI }) >>= documentURI
  assert $ uri == exampleURI

  pure unit
