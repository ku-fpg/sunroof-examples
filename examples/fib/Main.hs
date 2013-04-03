{-# LANGUAGE OverloadedStrings, ScopedTypeVariables #-}

module Main where

import Data.Default ( Default(..) )
import Data.Semigroup ( (<>) )
import Control.Monad ( liftM2 )
import Data.Boolean

import Language.Sunroof
import Language.Sunroof.Server
import Language.Sunroof.JS.Canvas
import Language.Sunroof.JS.Browser ( alert )
import Language.Sunroof.JS.JQuery
import Paths_sunroof_examples

main :: IO ()
main = do
 dataDir <- getDataDir
 sunroofServer (def { sunroofVerbose = 0
                      , cometResourceBaseDir = dataDir
                      , cometIndexFile = "examples/fib/index.html"
                      }) $ \ doc -> asyncJS doc prog


prog :: JSB ()
prog = do

      ch <- newChan

      jq "body" >>= on "click" ".click" (\ () -> do
                the_id :: JSString <- jq (cast $ this) >>= invoke "attr" ("id" :: JSString)
                o <- new "Object" ()
                o # "id" := the_id
                ch # writeChan o)
      jq "body" >>= on "slide" ".slide" (\ (a :: JSObject, aux :: JSObject) -> do
                the_id :: JSString <- jq (cast $ this) >>= invoke "attr" ("id" :: JSString)
                o <- new "Object" ()
                o # "id" := the_id
                o # "value" := (aux ! "value" :: JSString)
                ch # writeChan o)

      obj <- new "Object" ()
      obj # attr "model" := (0 :: JSNumber)

      -- This is using the imperative update to enable the
      let slider :: JSNumber -> JSObject -> JSB JSObject
          slider nm = invoke "slider"  ("value" :: JSString, nm)

          update :: String -> JSNumber -> JSNumber -> JSNumber -> JSB ()
          update nm val mn mx =
              ifB ((val <=* mx) &&* (val >=* mn))
                  (obj # attr nm := val)
                  (return ())

          switchB _   []         def = def
          switchB tag ((a,b):xs) def = ifB (tag ==* a) b (switchB tag xs def)

      fib <- liftJS $ fixJS $ \ fib -> function $ \ (n :: JSNumber) -> do
          ifB (n <* 2)
              (return (1 :: JSNumber))
              (liftM2 (+) (apply fib (n - 1)) (apply fib (n - 2)))

      loop () $ \() -> do
          res <- ch # readChan
--          res <- wait "body" (slide <> click)
          model <- evaluate (obj ! "model") :: JSB JSNumber

          switchB (res ! "id" :: JSString)
                  [ ("slider", update "model" (res ! "value") 0 25)
                  , ("up"    , update "model" (model + 1)     0 25)
                  , ("down"  , update "model" (model - 1)     0 25)
                  , ("reset" , update "model" 0               0 25)
                  ] $ return ()

          model <- evaluate (obj ! "model") :: JSB JSNumber
          jQuery "#slider"  >>= slider (cast model)
          liftJS $ do
                jQuery "#fib-out" >>= setHtml ("fib " <> cast model <> "...")
                res <- apply fib model
                jQuery "#fib-out" >>= setHtml ("fib " <> cast model <> " = " <> cast res)
                return ()

      return ()

default(JSNumber, JSString, String)


