module Main where

import Protolude hiding (link)

import Widgets
import Message (AppRequest)

import Reflex.Dom
import Reflex.Dom.WebSocket.Monad
import Reflex.Dom.WebSocket.Message

import Control.Lens
import Control.Monad.Fix

main = mainWidget $ do
  let url = "ws://localhost:3000/"
  withWSConnection
    url
    never -- close event
    True -- reconnect
    topWidget
  return ()

