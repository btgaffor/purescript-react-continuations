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
produce4 = cont $ \next -> next 4

------------------------------------------------------
main :: Effect Unit
main =
  flip runContT identity
    $ do
        first <- doPage "Choose first number:"
        second <- doPlusOne
        { first, second } <- doRecurse { first, second }
        pure $ mountApp $ React.createLeafElement resultClass { value: first + second, next: main }

doPage :: String -> ContT Unit Effect Int
doPage title = ContT $ \next -> mountApp (React.createLeafElement mainClass { title: title, next: next })

doRecurse :: { first :: Int, second :: Int } -> ContT Unit Effect { first :: Int, second :: Int }
doRecurse { first, second } = ContT $ doRecurseInner { first, second }

doRecurseInner :: { first :: Int, second :: Int } -> ({ first :: Int, second :: Int } -> Effect Unit) -> Effect Unit
doRecurseInner model next =
  mountApp
    $ DOM.div
        []
        [ DOM.h1 [] [ DOM.text $ show model.first <> ", " <> show model.second ]
        , DOM.button [ Props._type "button", Props.onClick $ \_ -> doRecurseInner (model { first = model.first + 1 }) next ] [ DOM.text "Add 1" ]
        , DOM.button [ Props._type "button", Props.onClick $ \_ -> doRecurseInner (model { second = model.second + 1 }) next ] [ DOM.text "Add 1" ]
        , DOM.button [ Props._type "button", Props.onClick $ \_ -> next model ] [ DOM.text "Submit" ]
        ]

doPlusOne :: ContT Unit Effect Int
doPlusOne = do
  second <- ContT $ \next -> mountApp (React.createLeafElement mainClass { title: "Choose second number: ", next: next })
  third <- pure 1
  pure $ second + third

mountApp :: React.ReactElement -> Effect Unit
mountApp app =
  void
    $ do
        window <- DOM.window
        document <- DOM.document window
        element <- DOM.getElementById "example" $ DOM.toNonElementParentNode document
        case element of
          Nothing -> error "Could not find DOM node to mount on" *> pure Nothing
          Just node -> ReactDOM.render app node

mainClass :: React.ReactClass { title :: String, next :: (Int -> Effect Unit) }
mainClass =
  React.component "Main" \this ->
    pure
      { state: {}
      , render:
        React.getProps this
          # map \{ title, next } ->
              DOM.div []
                [ DOM.h1 [] [ DOM.text title ]
                , DOM.div [] $ [ 1, 2, 3, 4, 5 ]
                    # map \option ->
                        React.createLeafElement optionButtonClass { option: option, next: next }
                ]
      }

optionButtonClass :: React.ReactClass { option :: Int, next :: (Int -> Effect Unit) }
optionButtonClass =
  React.component "OptionButton" \this ->
    pure
      { state: {}
      , render:
        React.getProps this
          # map \{ option, next } ->
              DOM.button
                [ Props._type "button", Props.onClick $ \_ -> next option ]
                [ DOM.int option ]
      }

resultClass :: React.ReactClass { value :: Int, next :: Effect Unit }
resultClass =
  React.component "Result" \this ->
    pure
      { state: {}
      , render:
        React.getProps this
          # map \{ value, next } ->
              DOM.div []
                [ DOM.h1 [] [ DOM.text $ "Result: " <> (show value) ]
                , DOM.button [ Props._type "button", Props.onClick $ \_ -> next ] [ DOM.text "Reset" ]
                ]
      }
