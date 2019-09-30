module Main where

import Prelude
import Effect (Effect)
import Effect.Console (error)
import Control.Monad.Cont

-- import Partial.Unsafe (unsafePartial)
import Data.Maybe (Maybe(..))

import Web.HTML.HTMLDocument (toNonElementParentNode) as DOM
import Web.DOM.NonElementParentNode (getElementById) as DOM
import Web.HTML (window) as DOM
import Web.HTML.Window (document) as DOM
import React.DOM (text, int, button, div, h1) as DOM
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
  flip runContT identity $ do
    first <- doPage "Choose first number:"
    second <- doPlusOne
    pure $ mountApp $ React.createLeafElement resultClass { value: first + second, next: main }

doPage :: String -> ContT Unit Effect Int
doPage title =
  ContT $ \next -> mountApp (React.createLeafElement mainClass { title: title, num: 3, next: next })

doPlusOne :: ContT Unit Effect Int
doPlusOne = do
  second <- ContT $ \next -> mountApp (React.createLeafElement mainClass { title: "Choose second number: ", num: 3, next: next })
  third <- pure 1
  pure $ second + third

mountApp :: React.ReactElement -> Effect Unit
mountApp app = void $ do
  window <- DOM.window
  document <- DOM.document window
  element <- DOM.getElementById "example" $ DOM.toNonElementParentNode document
  case element of
    Nothing -> error "Could not find DOM node to mount on" *> pure Nothing
    Just node -> ReactDOM.render app node

mainClass :: React.ReactClass { title :: String, num :: Int, next :: (Int -> Effect Unit) }
mainClass = React.component "Main" component
  where
  component this =
    pure { state: {}
          , render: render <$> React.getProps this
          }
    where
      render { title, num, next } =
        DOM.div []
          [ DOM.h1 [] [ DOM.text title ]
          , DOM.div [] $ map button [1, 2, 3, 4, 5]
          ]
        where
          button option = React.createLeafElement optionButtonClass { option: option, next: next }

optionButtonClass :: React.ReactClass { option :: Int, next :: (Int -> Effect Unit) }
optionButtonClass = React.component "OptionButton" component
  where
  component this =
    pure { state: {}
         , render: render <$> React.getProps this
         }
    where
    render { option, next } =
      DOM.button
        [ Props._type "button" , Props.onClick $ \_ -> next option]
        [ DOM.int option ]

resultClass :: React.ReactClass { value :: Int, next :: Effect Unit }
resultClass = React.component "Result" \this ->
  pure { state: {}
       , render: React.getProps this # map \{ value, next } ->
         DOM.div []
           [ DOM.h1 [] [ DOM.text $ "Result: " <> (show value) ]
           , DOM.button [ Props._type "button", Props.onClick $ \_ -> next ] [ DOM.text "Reset" ]
           ]
       }
