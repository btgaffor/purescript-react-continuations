module Main where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Effect.Console (log, error)
import Control.Monad.Cont
-- import Control.Monad.Eff

import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..), fromJust)

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import React.DOM (text, button) as DOM
import React.DOM.Props as Props

import React as React
import ReactDOM as ReactDOM

-- main :: Effect Unit
-- main = do
--   log $ runCont (doAdd produce3 produce4) show

runShow :: (Cont String Int) -> String
runShow c = flip runCont show $ c

doAdd :: forall r. (Cont r Int) -> (Cont r Int) -> Cont r Int
doAdd first second = do
  x <- first
  y <- second
  pure $ x + y

produce3 :: forall r. Cont r Int
produce3 = pure 3

produce4 :: forall r. Cont r Int
produce4 =
  cont $ \next -> next 4

------------------------------------------------------

main :: Effect Unit
-- main = mountApp (React.createLeafElement mainClass { num: 3 })
main =
  runContT continueWithApp $ \result -> mountApp (DOM.text $ show result)

continueWithApp :: ContT Unit Effect Int
continueWithApp =
  ContT $ \next -> mountApp (React.createLeafElement mainClass { num: 3, next: next })

mountApp :: React.ReactElement -> Effect Unit
mountApp app = void $ do
  window <- DOM.window
  document <- DOM.document window
  element <- DOM.getElementById "example" $ DOM.toNonElementParentNode document
  case element of
    Nothing -> error "Could not find DOM node to mount on" *> pure Nothing
    Just node -> ReactDOM.render app node

mainClass :: React.ReactClass { num :: Int, next :: (Int -> Effect Unit) }
mainClass = React.component "Main" component
  where
  component this =
    pure { state: {}
          , render: render <$> React.getProps this
          }
    where
      render { num, next } =
        DOM.button
          [ Props._type "button"
          , Props.onClick $ \_ -> next 3
          ]
          [ DOM.text $ runCont (doAdd produce3 produce4) $ \x -> show $ x + num ]
