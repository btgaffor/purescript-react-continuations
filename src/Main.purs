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
        result <- ContT $ flip doRecurse { first, second }
        pure $ mountApp $ React.createLeafElement resultClass { value: result.first + result.second, next: main }

doPage :: String -> ContT Unit Effect Int
doPage title = ContT $ \next -> mountApp (React.createLeafElement mainClass { title: title, next: next })

data Action
  = First Int
  | Second Int

type Model
  = { first :: Int, second :: Int }

updateRecurse :: Model -> Action -> Model
updateRecurse model action = case action of
  First num -> model { first = num }
  Second num -> model { second = num }

doRecurse :: (Model -> Effect Unit) -> Model -> Effect Unit
doRecurse next model =
  mountApp
    $ DOM.div
        []
        [ DOM.h1 [] [ DOM.text $ show model.first <> ", " <> show model.second ]
        , increaseButtonTagged model.first (updateRecurse model >>> doRecurse next) First
        , increaseButton model.second $ doRecurse next <<< model { second = _ }
        , DOM.button [ Props._type "button", Props.onClick $ const $ next model ] [ DOM.text "Submit" ]
        ]

increaseButtonTagged :: Int -> (Action -> Effect Unit) -> (Int -> Action) -> React.ReactElement
increaseButtonTagged value update tag = DOM.button [ Props._type "button", Props.onClick $ \_ -> update (tag (value + 1)) ] [ DOM.text "Add 1" ]

increaseButton :: Int -> (Int -> Effect Unit) -> React.ReactElement
increaseButton value next = DOM.button [ Props._type "button", Props.onClick $ const $ next (value + 1) ] [ DOM.text "Add 1" ]

doPlusOne :: ContT Unit Effect Int
doPlusOne = do
  second <- ContT $ \next -> mountApp (React.createLeafElement mainClass { title: "Choose second number: ", next: next })
  third <- pure 1
  pure $ second + third

mountApp :: React.ReactElement -> Effect Unit
mountApp app =
  void
    $ DOM.window
    >>= DOM.document
    >>= (DOM.toNonElementParentNode >>> pure)
    >>= DOM.getElementById "example"
    >>= \element -> case element of
        Nothing -> error "Could not find DOM node to mount on" *> pure Nothing
        Just node -> ReactDOM.render app node

mainClass :: React.ReactClass { title :: String, next :: (Int -> Effect Unit) }
mainClass =
  React.statelessComponent \{ title, next } ->
    DOM.div []
      [ DOM.h1 [] [ DOM.text title ]
      , DOM.div [] $ map ({ option: _, next } >>> React.createLeafElement optionButtonClass) [ 1, 2, 3, 4, 5 ]
      ]

optionButtonClass :: React.ReactClass { option :: Int, next :: (Int -> Effect Unit) }
optionButtonClass =
  React.statelessComponent \{ option, next } ->
    DOM.button
      [ Props._type "button", Props.onClick $ const $ next option ]
      [ DOM.int option ]

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
                , DOM.button [ Props._type "button", Props.onClick $ const next ] [ DOM.text "Reset" ]
                ]
      }
