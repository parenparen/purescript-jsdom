module DOM.JSDOM
  ( JSDOM
  , Callback
  , env
  , envAff
  , jsdom
  ) where

import Control.Monad.Aff (Aff, makeAff, nonCanceler)
import Control.Monad.Eff (kind Effect, Eff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn4, runEffFn2, runEffFn4, mkEffFn2)
import Data.Either (Either(..))
import Data.Maybe (maybe)
import Data.Nullable (Nullable, toMaybe)
import DOM.Node.Types (Document)
import DOM.HTML.Types (Window)
import Prelude (Unit, ($), discard, pure)

foreign import data JSDOM :: Effect

type JSCallback eff a = EffFn2 (jsdom :: JSDOM | eff) (Nullable Error) a  Unit
type Callback eff a = Either Error a -> Eff (jsdom :: JSDOM | eff) Unit

toJSCallback :: forall a eff. Callback eff a -> JSCallback eff a
toJSCallback f = mkEffFn2 (\e a -> f $ maybe (Right a) Left (toMaybe e))

foreign import _jsdom ::
  { env :: forall configs eff. EffFn4 (jsdom :: JSDOM | eff) String (Array String) { | configs} (JSCallback eff Window) Unit
  , jsdom :: forall configs eff. EffFn2 (jsdom :: JSDOM | eff) String { | configs} Document
  }

env :: forall configs eff. String -> Array String -> { | configs} -> Callback eff Window -> (Eff (jsdom :: JSDOM | eff) Unit)
env urlOrHtml scripts configs callback = runEffFn4 _jsdom.env urlOrHtml scripts configs (toJSCallback callback)

envAff :: forall configs eff. String -> Array String -> { | configs} -> Aff (jsdom :: JSDOM | eff) Window
envAff urlOrHtml scripts configs =
    makeAff \cb -> do
        env urlOrHtml scripts configs cb
        pure nonCanceler

jsdom :: forall configs eff. String -> { | configs} -> Eff (jsdom :: JSDOM | eff) Document
jsdom markup configs = runEffFn2 _jsdom.jsdom markup configs
