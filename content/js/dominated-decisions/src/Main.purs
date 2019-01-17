module Main where

import Prelude

import Effect (Effect)
import Effect.Console as Console
import Foreign (Foreign)

import JQuery as J

foreign import jQuery :: Foreign

main :: Effect Unit
main = do
  J.ready do
    Console.log "hello"
