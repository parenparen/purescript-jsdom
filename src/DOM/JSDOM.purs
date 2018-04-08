module DOM.JSDOM
  ( JSDOM
  , jsdom
  ) where

import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import DOM.Node.Types (Document)

foreign import data JSDOM :: Effect


-- Should really give a type to `configs`
foreign import _jsdom :: forall configs eff. EffFn2 (jsdom :: JSDOM | eff) String { | configs} Document


jsdom :: forall configs eff. String -> { | configs} -> Eff (jsdom :: JSDOM | eff) Document
jsdom markup configs = runEffFn2 _jsdom markup configs
