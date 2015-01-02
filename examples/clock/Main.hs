{-# LANGUAGE OverloadedStrings, DataKinds, ScopedTypeVariables #-}

module Main (main) where

import Prelude hiding (mod, div)

import Data.Monoid
import Data.Boolean
import Data.Boolean.Numbers
import Data.Default

import System.FilePath ( (</>) )

import Language.Sunroof
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser hiding ( eval )
import Language.Sunroof.JS.JQuery
import Language.Sunroof.JS.Date

import Paths_sunroof_examples

main :: IO ()
main = do
  -- Copy the index HTML and jquery.js to the current directory.
  dataDir <- getDataDir
  readFile (dataDir </> "js/jquery.js") >>= writeFile "jquery.js"
  readFile (dataDir </> "examples/clock/index.html") >>= writeFile "clock.html"
  -- Compile the JavaScript and also write it to the current directory.
  sunroofCompileJSA def "main" clockJS >>= writeFile "main.js"

clockJS :: JS 'A (JSFunction () ())
clockJS = function $ \() -> do

  -- Renders a single line (with number) of the clock face.
  renderClockFaceLine <- function $ \(c :: JSCanvas, u :: JSNumber, n :: JSNumber) -> do
    c # save
    -- Draw one of the indicator lines
    c # beginPath
    c # moveTo (0, -u * 1.0)
    ifB (n `mod` 5 ==* 0)
        (c # lineTo (0, -u * 0.8)) -- Minute line
        (c # lineTo (0, -u * 0.9)) -- Hour line
    ifB (n `mod` 15 ==* 0)
        (c # lineWidth := 8) -- Quarter line
        (c # lineWidth := 3) -- Non-Quarter line
    c # stroke
    c # closePath
    -- Draw of the hour numbers
    ifB (n `mod` 5 ==* 0)
        (do
          c # translate (-u * 0.75, 0)
          c # rotate (-2 * pi / 4)
          c # fillText (cast $ n `div` 5) (0, 0)
        ) (return ())
    c # restore

  -- Renders a single clock pointer.
  renderClockPointer <- function $ \(c :: JSCanvas, u :: JSNumber, angle :: JSNumber, width' :: JSNumber, len :: JSNumber) -> do
    c # save
    c # lineCap := "round"
    c # rotate angle
    c # lineWidth := width'
    c # beginPath
    c # moveTo (0, u * 0.1)
    c # lineTo (0, -u * len)
    c # stroke
    c # closePath
    c # restore
  -- Renders the clocks pointers for hours, minutes and seconds.
  renderClockPointers <- function $ \(c :: JSCanvas, u :: JSNumber) -> do
    (h, m, s) <- currentTime
    c # save
    c # lineCap := "round"
    -- Hour pointer
    renderClockPointer $$
      (c, u, (2 * pi / 12) * ((h `mod` 12) + (m `mod` 60) / 60), 15, 0.4)
    -- Minute pointer
    renderClockPointer $$
      ( c, u, (2 * pi / 60) * ((m `mod` 60) + (s `mod` 60) / 60), 10, 0.7)
    -- Second pointer
    c # strokeStyle := "red"
    renderClockPointer $$ ( c, u, (2 * pi / 60) * (s `mod` 60), 4, 0.9)
    -- Restore everything
    c # restore

  -- Renders the complete face of the clock, without pointers.
  renderClockFace <- function $ \(c :: JSCanvas, u :: JSNumber) -> do
    c # save
    c # rotate (2 * pi / 4) -- 0 degrees is at the top
    -- Draw all hour lines.
    lines' <- array [1..60::Int]
    lines' # forEach $ \n -> do
      c # save
      c # rotate ((2 * pi / 60) * n)
      renderClockFaceLine $$ (c, u, n)
      c # restore
    c # restore -- Undo all the rotation.

  -- Renders the complete clock.
  renderClock <- continuation $ \() -> do
    u <- clockUnit
    (w,h) <- canvasSize
    c <- context
    -- Basic setup
    c # save
    c # fillStyle := "black"
    c # strokeStyle := "black"
    c # lineCap := "round"
    c # textAlign := "center"
    c # font := ((cast $ u * 0.1) <> "px serif")
    c # textBaseline := "top"
    c # clearRect (0,0) (w,h)
    c # translate (w / 2, h / 2)
    -- Draw all hour lines.
    renderClockFace $$ (c, u)
    -- Draw the clock pointers
    renderClockPointers $$ (c, u)
    c # restore
    return ()

  _ <- window # setInterval (goto renderClock) 1000
  -- and draw one now, rather than wait till later
  _ <- goto renderClock ()

  return ()

canvas :: JS t JSObject
canvas = document # getElementById "canvas"

context :: JS t JSCanvas
context = canvas >>= getContext "2d"

clockUnit :: JS t JSNumber
clockUnit = do
  (w, h) <- canvasSize
  return $ (maxB w h) / 2

canvasSize :: JS t (JSNumber, JSNumber)
canvasSize = do
  c <- jQuery "#canvas"
  w <- c # invoke "innerWidth" ()
  h <- c # invoke "innerHeight" ()
  return (w, h)

currentTime :: JS t (JSNumber, JSNumber, JSNumber)
currentTime = do
  date <- newDate ()
  h <- date # getHours
  m <- date # getMinutes
  s <- date # getSeconds
  return (h, m, s)
