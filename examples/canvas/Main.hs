{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main (main) where

import Data.Default
import Data.Boolean

import Language.Sunroof
import Language.Sunroof.Server
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser
import Language.Sunroof.JS.JQuery
import Paths_sunroof_examples

-- This is a transcription of the http://www.html5canvastutorials.com/ demos/tutorials.

main :: IO ()
main = do
 dataDir <- getDataDir
 sunroofServer (def { sunroofVerbose = 0
                      , cometResourceBaseDir = dataDir
                      , cometIndexFile = "examples/canvas/index.html"
                      }) main2

main2 :: SunroofEngine -> IO ()
main2 doc = do

  ch <- rsyncJS doc $ newChan
  asyncJS doc $ do
          jq "body" >>= on "click" ".click" (\ () -> do
                  o <- new "Object" ()
                  o # "id" := ("click" :: JSString)
                  ch # writeChan o)

  sequence_ $ map (\ (ex,msg) -> do
      syncJS doc $ do
          canvas <- document # getElementById "canvas"
          c <- canvas # getContext "2d"
          c # clearRect (0,0) (canvas ! width, canvas ! height)
          forkJS $ do
                  ex canvas c
                  message canvas c msg
          -- wait for a click
          _ :: JSObject <- ch # readChan
          return ()) (cycle examples)

  --whenEvent doc "body" click
  --  $ syncJS doc $ onClick $ examples !! 0
  --sequence_ $ map (syncJS doc) $ map waitForClick $ examples
  --return ()

default(JSNumber, JSString, String)

examples :: [(JSObject -> JSCanvas -> JSA (), JSString)]
examples =
  [ (example_1_2_1,"1.2.1 Line")
  , (example_1_2_2,"1.2.2 Line Width")
  , (example_1_2_3,"1.2.3 Line Color")
  , (example_1_2_4,"1.2.4 Line Cap")
  , (example_1_3_1,"1.3.1 Arc")
  , (example_1_5_4,"1.5.4 Circle")
  , (example_1_8_1,"1.8.1 Text Font & Size")
  , (example_1_8_2,"1.8.2 Text Color")
  , (example_1_8_3,"1.8.3 Text Stroke")
  , (example_1_8_4,"1.8.4 Text Align")
  , (example_1_8_5,"1.8.5 Text Baseline")
  ]

example_1_2_1 :: JSObject -> JSCanvas -> JSA ()
example_1_2_1 _ c = do
  c # beginPath
  c # moveTo (100,150)
  c # lineTo (450,50)
  c # closePath
  c # stroke

example_1_2_2 :: JSObject -> JSCanvas -> JSA ()
example_1_2_2 _ c = do
  c # beginPath
  c # moveTo (100,150)
  c # lineTo (450,50)
  c # closePath
  c # lineWidth :=  15
  c # stroke

example_1_2_3 :: JSObject -> JSCanvas -> JSA ()
example_1_2_3 _ c = do
  c # beginPath
  c # moveTo (100,150)
  c # lineTo (450,50)
  c # closePath
  c # lineWidth := 5
  c # strokeStyle := "#ff0000"
  c # stroke

example_1_2_4 :: JSObject -> JSCanvas -> JSA ()
example_1_2_4 canvas c = do
  w <- evaluate $ canvas ! width
  h <- evaluate $ canvas ! height
  sequence_
    [ do c # beginPath
         c # moveTo (200, h / 2 + n)
         c # lineTo (w - 200, h / 2 + n)
         c # closePath
         c # lineWidth := 20
         c # strokeStyle := "#0000ff"
         c # lineCap := cap
         c # stroke
    | (cap,n) <- zip ["butt","round","square"] [-50,0,50]
    ]

example_1_3_1 :: JSObject -> JSCanvas -> JSA ()
example_1_3_1 canvas c = do
  w <- evaluate $ canvas ! width
  h <- evaluate $ canvas ! height
  let centerX = w / 2;
  let centerY = h / 2;
  let radius = 75;
  let startingAngle = 1.1 * pi
  let endingAngle = 1.9 * pi
  let counterclockwise = false
  c # beginPath
  c # arc' (centerX, centerY) radius (startingAngle, endingAngle) counterclockwise
  c # closePath
  c # lineWidth := 15
  c # strokeStyle := "black"
  c # stroke

example_1_5_4 :: JSObject -> JSCanvas -> JSA ()
example_1_5_4 canvas c = do
  w <- evaluate $ canvas ! width
  h <- evaluate $ canvas ! height
  let centerX = w / 2
  let centerY = h / 2
  let radius = 70
  c # beginPath
  c # arc' (centerX, centerY) radius (0, 2 * pi) false
  c # fillStyle := "#8ED6FF"
  c # fill
  c # lineWidth := 5
  c # strokeStyle := "black"
  c # stroke

example_1_8_1 :: JSObject -> JSCanvas -> JSA ()
example_1_8_1 _ c = do
  c # font := "40pt Calibri"
  c # fillText "Hello World!" (150, 100)

example_1_8_2 :: JSObject -> JSCanvas -> JSA ()
example_1_8_2 _ c = do
  c # font := "40pt Calibri"
  c # fillStyle := "#0000ff"
  c # fillText "Hello World!" (150, 100)

example_1_8_3 :: JSObject -> JSCanvas -> JSA ()
example_1_8_3 _ c = do
  c # font := "60pt Calibri"
  c # lineWidth := 3
  c # strokeStyle := "blue"
  c # strokeText "Hello World!" (80, 110)

example_1_8_4 :: JSObject -> JSCanvas -> JSA ()
example_1_8_4 canvas c = do
  w <- evaluate $ canvas ! width
  h <- evaluate $ canvas ! height
  let x = w / 2
  -- Draw alignment line
  c # strokeStyle := "red"
  c # beginPath
  c # moveTo (x, 0)
  c # lineTo (x, h)
  c # closePath
  c # lineWidth := 1
  c # stroke
  -- Draw text
  c # font := "30px Calibri"
  c # textBaseline := "top"
  c # fillStyle := "blue"
  -- Function to draw baseline identifier on its baseline.
  let textFun :: JSString -> JSNumber -> JSA JSNumber
      textFun al offset = do
        c # textAlign := al
        c # fillText al (x, offset)
        return $ offset + 30
  -- Line the different identifiers up after each other.
  passFold_ 0 $ map textFun ["center", "end", "left", "right", "start"]

example_1_8_5 :: JSObject -> JSCanvas -> JSA ()
example_1_8_5 canvas c = do
  w <- evaluate $ canvas ! width
  h <- evaluate $ canvas ! height
  let y = h / 2
  -- Draw baseline
  c # strokeStyle := "red"
  c # beginPath
  c # moveTo (0, y)
  c # lineTo (w, y)
  c # closePath
  c # lineWidth := 1
  c # stroke
  -- Draw text
  c # font := "15pt Calibri"
  c # textAlign := "left"
  c # fillStyle := "blue"
  -- Function to draw baseline identifier on its baseline.
  let textFun :: JSString -> JSNumber -> JSA JSNumber
      textFun bl offset = do
        c # textBaseline := bl
        c # fillText bl (offset, y)
        tm <- c # measureText bl
        return $ offset + (tm ! width)
  -- Line the different identifiers up after each other.
  passFold_ 0 $ map textFun [ "alphabetic"
                            , "top"
                            , "hanging"
                            , "middle"
                            , "ideographic"
                            , "bottom"]

passFold :: (Monad m) => a -> [a -> m a] -> m a
passFold e (k : ks) = k e >>= \e' -> passFold e' ks
passFold e [] = return e

passFold_ :: (Monad m) => a -> [a -> m a] -> m ()
passFold_ e l = passFold e l >> return ()

message :: JSObject -> JSCanvas -> JSString -> JSA ()
message canvas c msg = do
  c # save
  c # font := "30pt Calibri"
  c # textAlign := "left"
  c # textBaseline := "alphabetic"
  c # fillStyle := "#8090a0"
  c # fillText msg (10, (canvas ! height) - 10)
  c # restore
