{-# LANGUAGE CPP, OverloadedStrings, ForeignFunctionInterface #-}
module Main where

import JavaScript.Canvas
import JavaScript.JQuery
import GHCJS.Types
import GHCJS.Foreign
import Control.Concurrent

main = do
  ctx <- getContext =<< indexArray 0 . castRef =<< select "#theCanvas"
  forkIO $ mapM_ (frame ctx) (cycle [0,0.05..pi/2])
  return ()

frame :: Context -> Double -> IO ()
frame ctx rotation = do
  save ctx
  fillStyle 255 255 255 1 ctx
  fillRect 0 0 200 200 ctx
  restore ctx
  save ctx
  translate 100 100 ctx
  rotate rotation ctx
  strokeRect (-50) (-50) 100 100 ctx
  restore ctx
  threadDelay 50000

