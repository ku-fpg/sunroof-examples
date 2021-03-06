{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE ImpredicativeTypes #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Main (main) where

import Prelude.Compat as Prelude hiding
    ( mod
    , div
    , (<*)
    )

import Data.Semigroup
import Data.Boolean
import Data.Boolean.Numbers hiding (floor, round)
import Data.Default.Class
import Data.Maybe ( isJust )
import qualified Data.Map as Map

import qualified Numeric

import Language.Sunroof as SR
import Language.Sunroof.Server
import Language.Sunroof.JS.JQuery (jQuery)
import qualified Language.Sunroof.JS.JQuery as JQuery
import qualified Language.Sunroof.JS.Browser as B

import Language.Sunroof.JS.Array as A
import qualified Language.Sunroof.JS.Map as M

import System.IO
import Control.Monad (when, liftM2)

import Test.QuickCheck
import Test.QuickCheck.Monadic ( run, monadicIO, assert, pick, pre )
import Test.QuickCheck.Gen ( Gen(MkGen, unGen) )
import Test.QuickCheck.Property hiding (Result,reason)
import qualified Test.QuickCheck.Property as P
--  ( callback, abort, ok
--  , Callback( PostTest )
--  , CallbackKind( NotCounterexample )
--  )
import Test.QuickCheck.State ( State( .. )) -- numSuccessTests ) )

import Control.Concurrent.ParallelIO.Local hiding (parallelInterleaved)
import Control.Concurrent.ParallelIO.Local (parallelInterleaved)
import qualified Control.Exception as E
import Paths_sunroof_examples

main :: IO ()
main = do
   dataDir <- getDataDir
   sunroofServer (def { sunroofVerbose = 0
                      , cometResourceBaseDir = dataDir
                      , cometIndexFile = "examples/unit/index.html"
                      }) $ \ doc0 -> do
        let do_log = False
        let te_style = TestWithTiming
--        let te_style = TestInPar 4
        doc <- case te_style of
                  TestWithTiming -> newTimings doc0
                  _ -> return doc0
        web_app $ TestEngine doc do_log te_style False (5 * 1000 * 1000)

default(JSNumber, JSString, String)

type instance BooleanOf () = JSBool

data TestEngine = TestEngine { srEngine :: SunroofEngine
                             , teLog    :: Bool                 -- do you send information about each test to a log
                             , teStyle  :: TestStyle
                             , teShrink :: Bool                 -- do you want to shrink on failure?
                             , teTimeout :: Int                 -- millseconds timeout failure for each single test
                             }

data TestStyle = TestWithTiming         -- single core, do timing
               | TestInPar Int          -- How many cores
               deriving (Show, Eq)

-- This is run each time the page is first accessed
web_app :: TestEngine -> IO ()
web_app doc = do

        let tA = ThreadProxy :: ThreadProxy 'A
        let tB = ThreadProxy :: ThreadProxy 'B

        runTests doc $ take 100 $ drop 0 $
          [ ("Constants",
                [ Test 100 "Constant Numbers" (checkConstNumber doc :: Double -> Property)
-- Comment out until we return SunroofArgument, vs just Sunroofs.
--                , Test 100 "Constant Unit"    (checkConstValue doc :: () -> Property)
                , Test  10 "Constant Boolean" (checkConstValue doc :: Bool -> Property)
                , Test 100 "Constant String"  (checkConstValue doc :: String -> Property)
                ])
          , ("Arithmetic and Booleans",
                [ Test 100 "Basic Addition"       (checkBasicArith doc (+) :: Double -> Double -> Property)
                , Test 100 "Basic Subtraction"    (checkBasicArith doc (-) :: Double -> Double -> Property)
                , Test 100 "Basic Multiplication" (checkBasicArith doc (*) :: Double -> Double -> Property)
                , Test 100 "Arbitrary Arithmetic" (checkArbitraryArith doc)
                , Test 100 "Arbitrary Boolean"    (checkArbitraryBool  doc)
                ])
          , ("Conditionals",
                [ Test  10 "if/then/else -> Int       (A)"   (checkArbitraryIfThenElse_Int doc tA)
                , Test  10 "if/then/else -> Int       (B)"   (checkArbitraryIfThenElse_Int doc tB)
                , Test  10 "if/then/else -> ()        (A)"   (checkArbitraryIfThenElse_Unit doc tA)
                , Test  10 "if/then/else -> ()        (B)"   (checkArbitraryIfThenElse_Unit doc tB)
                , Test  10 "if/then/else -> (Int,Int) (A)"   (checkArbitraryIfThenElse_Int_Int doc tA)
                , Test  10 "if/then/else -> (Int,Int) (B)"   (checkArbitraryIfThenElse_Int_Int doc tB)
                ])
          , ("Uplink & Downlink",
                [ Test 100 "Constant String"   (checkDownlinkUplink' doc (==) :: String -> Property)
                , Test 100 "Constant Booleans" (checkDownlinkUplink' doc (==) :: Bool -> Property)
                , Test 100 "Constant Numbers"  (checkDownlinkUplink' doc deltaEqual :: Double -> Property)
                ])
          , ("Data Structures",
                [ Test 100 "Array"                    (checkArbitraryArray doc)
                , Test 100 "Map"                      (checkArbitraryMap doc)
                ]
            )
          , ("Channels and MVars",
                [ Test  10 "Chan (rand)"              (checkArbitraryChan_Int doc False SR.newChan SR.writeChan SR.readChan)
                , Test  10 "Chan (write before read)" (checkArbitraryChan_Int doc True SR.newChan SR.writeChan SR.readChan)
                , Test  1  "Chan (Empty)"             (checkMVars doc 3 SR.writeChan $ SR.newChan)
                , Test  10 "MVar (rand)"              (checkArbitraryChan_Int doc False SR.newEmptyMVar SR.putMVar SR.takeMVar)
                , Test  1  "MVar (Empty)"             (checkMVars doc 1 SR.putMVar $ SR.newEmptyMVar)
                , Test  1  "MVar (Full)"              (checkMVars doc 0 SR.putMVar $ SR.newMVar (-1))
                , Test  1  "MVar (Empty + put)"       (checkMVars doc 0 SR.putMVar $
                                                                           do { v <- SR.newEmptyMVar
                                                                              ; v # SR.putMVar (-1)
                                                                              ; return v })
                , Test  1  "MVar (Full + take)"       (checkMVars doc 1 SR.putMVar $
                                                                           do { v <- SR.newMVar (-1)
                                                                              ; _ <- v # SR.takeMVar
                                                                              ; return v })
                ])
          , ("Regression",
                [ Test 1 "Issue #29: Assignment Bug" (regressionAssignmentIssue29 doc)
                ])
          , ("Performance",
                [ Test   1 ("Fib " ++ show n)           (runFib doc n) | n <- [10 ] ++ [30 .. 35]
                ])
          ]


-- -----------------------------------------------------------------------
-- Tests
-- -----------------------------------------------------------------------

-- | Check if a constant literal value is the same after sync.
checkConstValue :: ( Eq a
                   , SunroofValue a
                   , SunroofResult (ValueOf a)
                   , Sunroof (ValueOf a)
                   , a ~ ResultOf (ValueOf a)
                   ) => TestEngine -> a -> Property
checkConstValue doc n = monadicIO $ do
  n' <- run $ syncJS (srEngine doc) (return $ js n)
  assert $ n == n'

-- | Check if a constant literal number is the same after sync.
checkConstNumber :: TestEngine -> Double -> Property
checkConstNumber doc n = monadicIO $ do
  n' <- run $ syncJS (srEngine doc) (return $ js n)
  -- Some weird conversion error going on. The returned value has more digits!
  assert $ n `deltaEqual` n'

-- | Check if simple arithmetic expressions with one operator produce
--   the same value after sync.
checkBasicArith :: TestEngine -> (forall b. (Num b) => b -> b -> b) -> Double -> Double -> Property
checkBasicArith doc op x y = monadicIO $ do
  let r = (x `op` y)
  r' <- run $ syncJS (srEngine doc) (return (js x `op` js y :: JSNumber))
  assert $ r `deltaEqual` r'

-- | Check if arithmetic expressions of arbitrary size produce the same result
--   after sync.
checkArbitraryArith :: TestEngine -> Int -> Property
checkArbitraryArith doc seed = monadicIO $ do
  let n = (abs seed `mod` 8) + 1
  (r, e) <- pick $ sameSeed (numExprGen n :: Gen Double)
                            (numExprGen n :: Gen JSNumber)
  pre $ abs r < (100000000 :: Double)
  r' <- run $ syncJS (srEngine doc) (return e)
  assert $ r `deltaEqual` r'

checkArbitraryBool :: TestEngine -> Int -> Property
checkArbitraryBool doc seed = monadicIO $ do
  let n = (abs seed `mod` 8) + 1
  (b, e) <- pick $ sameSeed (boolExprGen n :: Gen Bool)
                            (boolExprGen n :: Gen JSBool)
  b' <- run $ syncJS (srEngine doc) (return e)
  assert $ b == b'

-- TODO: add an effect to the if/then/else's
checkArbitraryIfThenElse_Int :: forall t . (SunroofThread t) => TestEngine -> ThreadProxy t -> Int -> Property
checkArbitraryIfThenElse_Int doc ThreadProxy seed = monadicIO $ do
  let n = (abs seed `mod` 8) + 1
  (b, e) <- pick $ sameSeed (boolExprGen n :: Gen Bool)
                            (boolExprGen n :: Gen JSBool)
  (r1, e1) <- pick $ sameSeed (numExprGen n :: Gen Double)
                              (numExprGen n :: Gen JSNumber)
  (r2, e2) <- pick $ sameSeed (numExprGen n :: Gen Double)
                              (numExprGen n :: Gen JSNumber)
  pre $ abs r1 < (100000000 :: Double)
  pre $ abs r2 < (100000000 :: Double)
--  run $ print ("e,e1,e2",e,e1,e2)
  r12' <- run $ syncJS (srEngine doc) (ifB e (return e1) (return e2) >>= return :: JS t JSNumber)
  assert $ (if b then r1 else r2) == r12'

-- TODO: add an effect to the if/then/else's
checkArbitraryIfThenElse_Int_Int :: forall t . (SunroofThread t) => TestEngine -> ThreadProxy t -> Int -> Property
checkArbitraryIfThenElse_Int_Int doc ThreadProxy seed = monadicIO $ do
  let n = (abs seed `mod` 8) + 1
  (b, e) <- pick $ sameSeed (boolExprGen n :: Gen Bool)
                            (boolExprGen n :: Gen JSBool)
  (r1, e1) <- pick $ sameSeed (numExprGen n :: Gen Double)
                              (numExprGen n :: Gen JSNumber)
  (r2, e2) <- pick $ sameSeed (numExprGen n :: Gen Double)
                              (numExprGen n :: Gen JSNumber)
  pre $ abs r1 < (1000000 :: Double)
  pre $ abs r2 < (1000000 :: Double)
--  run $ print ("e,e1,e2",e,e1,e2)
  -- TODO lift the restriction about returning tuples
  r12' <- run $ syncJS (srEngine doc) (
                do (x1,x2) <- ifB e (return (e1,e2)) (return (e2,e1))
                   return (x1 * 100 + x2) :: JS t JSNumber)
  assert $ (if b then (r1 * 100 + r2) else (r2 * 100 + r1)) == r12'



checkArbitraryIfThenElse_Unit :: forall t . (SunroofThread t) => TestEngine -> ThreadProxy t -> Int -> Property
checkArbitraryIfThenElse_Unit doc ThreadProxy seed = monadicIO $ do
  let n = (abs seed `mod` 8) + 1
  (_b, e) <- pick $ sameSeed (boolExprGen n :: Gen Bool)
                             (boolExprGen n :: Gen JSBool)
--  run $ print ("e,e1,e2",e,e1,e2)
  r12' <- run $ syncJS (srEngine doc) (ifB e (return ()) (return ()) >>= return :: JS t ())
  assert $ () == r12'

{-
checkArbitraryArray_Int
checkArbitraryArray_Int doc seed = monadicIO $ do
  let n = (abs seed `mod` 10) + 1
  sz <- pick $ choose (0,100)
  dat  :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector sz
-}

checkDownlinkUplink' :: forall a .
                        ( SunroofValue a
                        , Sunroof (ValueOf a)
                        , SunroofArgument (ValueOf a)
                        , SunroofResult (ValueOf a)
                        , a ~ ResultOf (ValueOf a)
                        ) => TestEngine
                          -> (a -> a -> Bool)
                          -> a
                          -> Property
checkDownlinkUplink' doc equals _value = monadicIO $ do
  (down :: Downlink (ValueOf a)) <- run $ newDownlink (srEngine doc)
  (up :: Uplink (ValueOf a)) <- run $ newUplink (srEngine doc)
  return $ checkDownlinkUplink doc equals down up

checkDownlinkUplink :: ( SunroofValue a
                       , Sunroof (ValueOf a)
                       , SunroofArgument (ValueOf a)
                       , SunroofResult (ValueOf a)
                       , a ~ ResultOf (ValueOf a)
                       ) => TestEngine
                         -> (a -> a -> Bool)
                         -> Downlink (ValueOf a)
                         -> Uplink (ValueOf a)
                         -> a
                         -> Property
checkDownlinkUplink doc equals down up value' = monadicIO $ do
  run $ putDownlink down (return $ js value')
  run $ asyncJS (srEngine doc) $ do
    v <- getDownlink down
    up # putUplink v
  value'' <- run $ getUplink up
  assert $ value' `equals` value''

checkArbitraryChan_Int
        :: TestEngine
        -> Bool -- write before any read
        -> (JS 'B (m JSNumber))
        -> (JSNumber -> m JSNumber -> JS 'B ())
        -> (m JSNumber -> JS 'B JSNumber)
        -> Int
        -> Property
checkArbitraryChan_Int doc wbr newChan' writeChan' readChan' _seed = monadicIO $ do
--   let n = (abs seed `mod` 8) + 1
  qPush <- pick $ frequency [(1,return False),(3,return True)]
  qPull <- pick $ frequency [(1,return False),(3,return True)]
  arr1 :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 10
  arr2 :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 10
  dat  :: [Int] <- fmap (fmap (`Prelude.rem` 100)) $ pick $ vector 10

  let prog :: JS 'B (JSArray JSNumber)
      prog = do
          note :: JSArray JSBool <- newArray ()
          ch <- newChan'
          (if wbr then id else forkJS) $
                   sequence_ [ do ifB (js (x >= 0 && qPush)) (SR.threadDelay (js x)) (return ())
                                  _ <- note # A.push true
                                  ch # writeChan' (js y :: JSNumber)
                             | (x,y) <- arr1 `zip` dat
                             ]
          arr :: JSArray JSNumber <- newArray ()
          sequence_ [ do ifB (js (x >= 0 && qPull)) (SR.threadDelay (js x)) (return ())
                         _ <- note # A.push false
                         z <- ch # readChan'
                         arr # A.push z
                    | x <- arr2
                    ]

          when (teLog doc) $ do
                 -- debugging Glyph; perhaps send to Haskell-land,
                 -- or somehow print on the screen?
                 B.console # B.log (mconcat [ ifB (note ! index (js n))
                                                  (">"::JSString)
                                                  "<"
                                            | n <- [0..19::Int]
                                            ])


          return arr
  res :: [Double] <- run $ syncJS (srEngine doc) prog
  assert $ map round res == dat

checkMVars
        :: TestEngine
        -> Int
        -> (JSNumber -> m JSNumber -> JS 'B ())
        -> (JS 'B (m JSNumber))
        -> Int
        -> Property
checkMVars doc sz write start _seed = monadicIO $ do

  res <- run $ syncJS (srEngine doc) $ do
     st :: JSRef JSNumber <- newJSRef 0
     -- how many pushes can a var do?
     var  <- start

     forkJS $ do
          st # writeJSRef 0
          var # write 0
          st # writeJSRef 1
          var # write 1
          st # writeJSRef 2
          var # write 2
          st # writeJSRef 3

     SR.threadDelay 1000
     res <- st # readJSRef

     when (teLog doc) $ do
                 B.console # B.log
                        (("checkMVars: " <> " expecting " <> cast (js sz :: JSNumber) <> ", found " <> cast res) :: JSString)

     return $ res

  run $ print res
  assert $ round res == sz


checkArbitraryArray
        :: TestEngine
        -> Property
checkArbitraryArray doc = monadicIO $ do
  (cons,ops) :: (ArrayConstructor SmallNat,[ArrayOp SmallNat]) <- pick (genArrayOps (10,10))

--  run $ print (cons,ops)

  res :: Bool <- run $ syncJS (srEngine doc) $ do
        arr <- case cons of
                 NewEmptyArray -> empty
                 NewArray xs   -> array (fmap (\ (SmallNat n) -> n) xs)
        let km = foldr (\ (op :: ArrayOp SmallNat) (km' :: JSA (JSFunction () JSBool)) -> do
                           function $ \ () -> do
                               k <- km'
                               case op of
                                 LookupArray n ok' -> do
                                   v <- evaluate $ lookup' (js n) arr
                                   case ok' of
                                     Val n' -> do
                                          ifB (js n' /=* v)
                                              (return false)
                                              (k $$ ())
                                     _ -> ifB (cast v /=* object "undefined")
                                              (return false)
                                              (k $$ ())
                                 InsertArray key v -> do
                                   arr # insert' (js key) (js v)
                                   k $$ ()
                                 LengthArray n -> do
                                   v <- evaluate $ arr ! A.length'
                                   ifB (v /=* js n)
                                       (return false)
                                       (k $$ ())
                                 ElemsArray xs -> do
                                   bs <- sequence [ do v <- evaluate $ arr ! index (js n)
                                                       case x of
                                                         Val v' -> return (v ==* js v')
                                                         _      -> return (cast v ==* object "undefined")
                                                  | (n :: Int,x) <- [0..] `zip` xs ]
                                   ifB (foldr (&&*) true bs)
                                       (k $$ ())
                                       (return false)
                         )
                         (function $ \ () -> return true)
                         ops
{-
                a        sequence [ case op of
                   data ArrayOp n
        = LookupArray Int               (Maybe (Val n))  -- n is the expected result
        | InsertArray Int n
        | LengthArray                   Int                 -- number of elements
        | ElemsArray                    [Val n]

                 | op <- ops
                 ]
-}
        return ()
        k <- km
        -- returns true or false
        k $$ ()

  assert $ res

checkArbitraryMap
        :: TestEngine
        -> Property
checkArbitraryMap doc = monadicIO $ do
  ops :: [MapOp SmallString SmallNat] <- pick (genMapOps 10)

  run $ print ops

  res :: Bool <- run $ syncJS (srEngine doc) $ do
        mp <- M.newMap
        let km = foldr (\ (op :: MapOp SmallString SmallNat) (km' :: JSA (JSFunction () JSBool)) -> do
                           function $ \ () -> do
                               k <- km'
                               case op of
                                 LookupMap key ok' -> do
                                   v <- M.lookup (js key) mp
                                   case ok' of
                                     Val n' -> do
                                          ifB (js n' /=* v)
                                              (return false)
                                              (k $$ ())
                                     _ -> ifB (cast v /=* object "undefined")
                                              (return false)
                                              (k $$ ())
                                 InsertMap key v -> do
                                   mp # M.insert (js key) (js v)
                                   k $$ ()
                                 SizeMap n -> do
                                   v <- M.size mp
                                   ifB (v /=* js n)
                                       (return false)
                                       (k $$ ())
{-
                                 ElemsMap xs -> do
                                   arr <- M.elems mp
                                   bs <- sequence [ do v <- evaluate $ arr ! index (js n)
                                                       case x of
                                                         Val v' -> return (v ==* js v')
                                                         _      -> return (cast v ==* object "undefined")
                                                  | (n :: Int,x) <- [0..] `zip` xs ]
                                   ifB (foldr (&&*) true bs)
                                       (k $$ ())
                                       (return false)
-}
                         )
                         (function $ \ () -> return true)
                         ops
        return ()
        k <- km
        -- returns true or false
        k $$ ()

  assert $ res
-- | Check if simple arithmetic expressions with one operator produce
--   the same value after sync.
runFib :: TestEngine -> Int -> Property
runFib doc n = monadicIO $ do
  r' <- run $ syncJS (srEngine doc) $ do
        fib <- fixJS $ \ fib -> function $ \ (n' :: JSNumber) -> do
                ifB (n' <* 2)
                    (return (1 :: JSNumber))
                    (liftM2 (+) (apply fib (n' - 1)) (apply fib (n' - 2)))
        apply fib (js n)
  let fib :: Int -> Int
      fib n' = xs !! n'
      xs = map (\ n' -> if n' < 2 then 1 else fib (n'-1) + fib (n'-2)) [0..]
  let r = fromIntegral (fib n)
  assert $ r `deltaEqual` r'

-- -----------------------------------------------------------------------
-- Regression Tests
-- -----------------------------------------------------------------------

-- | Regression test for the problem that occured in issue 29
--   (<https://github.com/ku-fpg/sunroof-compiler/issues/29>).
regressionAssignmentIssue29 :: TestEngine -> Property
regressionAssignmentIssue29 doc = monadicIO $ do
  () <- run $ syncJS (srEngine doc) $ do
    _v :: JSRef (JSContinuation ()) <- newJSRef (cast nullJS)
    -- Cause of Issue 29:
    -- Produces: function() { return (v86873["val"])(); } = null;
    -- Instead of: v86873["val"] = null;
    return ()
  return ()
    --s <- newJSRef start

-- -----------------------------------------------------------------------
-- Test execution
-- -----------------------------------------------------------------------

data Test = forall a. Testable a => Test Int String a

runTests :: TestEngine -> [(String,[Test])] -> IO ()
runTests doc all_tests = do
  syncJS (srEngine doc) $ do
          -- Set the fatal callback to continue, because we are testing things.
          fatal <- function $ \ (_a::JSObject,_b::JSObject,_c::JSObject,f::JSFunction () ()) ->
                        forkJS $ do
                                -- This should be a command line thing
--                                B.alert("FAILURE" <> cast a <> cast b <> cast c)
                                -- wait a second before retrying
                                SR.threadDelay 1000
                                apply f ()
          () <- fun "$.kc.failure"  `apply` fatal
          return ()


  let section title body = do
          asyncJS (srEngine doc) $ do
                   jQuery "#testing-text" >>= JQuery.append (cast $ js $
                          "<h1>" ++ title ++ "</h1>" ++ "<table>" ++ body ++ "</table>")
                   return ()

  sequence_ [ do section txt $ concat
                              [ "<tr class=\"" ++ pbName i j ++ "\">" ++
                                "<td class=\"count\">" ++ {-show n-} show (0::Int) ++ "</td>" ++
                                "<td class=\"progress\"><div class=\"progressbar\"> </div></td><th>"
                                        ++ msg ++ "</th>" ++
                                                "<td class=\"data data1\"></td>" ++
                                                "<td class=\"data data2\"></td>" ++
                                                "<td class=\"data data3\"></td>" ++
                                                "<td class=\"space\">&nbsp;</td>" ++
                                                "</tr>"
                              | (j::Int,Test _n msg _) <- [1..] `zip` tests
                              ]
           | (i::Int,(txt,tests)) <- [1..] `zip` all_tests
           ]

  section "Summary" $ "<tr class=\"" ++ pbName 0 0 ++ "\">" ++
                                "<td class=\"count\"></td>" ++
                                "<td class=\"progress\"></td><th></th>" ++
                                "<td class=\"data data1\"></td>" ++
                                "<td class=\"data data2\"></td>" ++
                                "<td class=\"data data3\"></td>" ++
                                "<td class=\"space\">&nbsp;</td>" ++
                                "</tr>" ++
                                "<tr>" ++
                                "<td class=\"count\"></td>" ++
                                "<td class=\"progress\"></td><th></th>" ++
                                "<td class=\"data data1\">(compile)</td>" ++
                                "<td class=\"data data2\">(send)</td>" ++
                                "<td class=\"data data3\">(run)</td>" ++
                                "<td class=\"space\">&nbsp;</td>" ++
                                "</tr>"

  let casesPerTest :: Int
      casesPerTest = 100

  -- set them all to 100 max
  asyncJS (srEngine doc) $ do
    () <- jQuery ".progressbar" >>= invoke "progressbar" ()  :: JS t ()
    () <- jQuery ".progressbar" >>= invoke "progressbar" ( "option" :: JSString
                                                   , "max" :: JSString
                                                   , js casesPerTest :: JSNumber
                                                   )
    () <- jQuery ".progressbar" >>= invoke "progressbar" ( "value" :: JSString
                                                   , 0 :: JSNumber
                                                   )
    return ()


  result' <- (case teStyle doc of
                TestInPar n -> \ xs -> withPool n $ \ pool -> parallelInterleaved pool xs
                _ -> sequence) $ concat [
      [ do let runTest :: Test -> IO (Result,Timings Double)
               runTest (Test count name test) = do
                 resetTimings (srEngine doc)
                 putStrLn name
                 r <- quickCheckWithResult (stdArgs {chatty=False,maxSuccess=count})
                   $ within (teTimeout doc)
                   $ (if teShrink doc then id else noShrinking)
                   $ callback (afterTestCallback count)
                   $ test
                 t' <- getTimings (srEngine doc)
                 putStrLn "DONE TESTS IN SR"
                 return (r,fmap realToFrac t')
               execTest :: Test -> IO (Maybe (Timings Double))
               execTest t'@(Test _ name _) = do
--                 progressMsg doc name
                 result'' <- E.try (runTest t' >>= E.evaluate)
                 case result'' of
                   Left (e ::  E.SomeException) -> do
                     print ("EXCEPTION:"::String,e)
                     overwriteMessage doc i j ("Exception!") "failure"
                     E.throw e
                   Right (Success _ _ out,t'') -> do
                     putStrLn out
                     overwriteMessage doc i j ("Passed") "success"
                     writeTimings doc i j t''
                     return $ Just t''
                   Right (GaveUp _ _ out,_) -> do
                     putStrLn out
                     overwriteMessage doc i j ("Gave up") "failure"
                     return Nothing
                   Right (f@Failure {},_) -> do
--                     putStrLn (output f)
--                     putStrLn (reason f)
                     putStrLn $ "FAILED TEST: " ++ name
                     overwriteMessage doc i j ("Failed: " ++ reason f) "failure"
                     -- carry on, please
                     return Nothing     -- failure
                   Right (NoExpectedFailure _ _ out,_) -> do
                     putStrLn out
                     overwriteMessage doc i j ("Ho expected failure") "failure"
                     return Nothing
               afterTestCallback :: Int -> Callback
               afterTestCallback count = PostTest NotCounterexample $ \ state result'' -> do
                 if not (P.abort result'') && isJust (ok result'')
                   then do
                     progressVal doc i j (numSuccessTests state + 1) (((numSuccessTests state + 1) * 100) `div` count)
                     if numSuccessTests state `mod` (casesPerTest `div` 10) == 0
                       then do
                         putStr "."
                         hFlush stdout
                       else return ()
                   else do
                     return ()
           execTest t

      | (j::Int,t@(Test _ _ _)) <- [1..] `zip` tests
      ]
    | (i::Int,(_txt,tests)) <- [1..] `zip` all_tests
    ]

  asyncJS (srEngine doc) $ do
    p <- pbObject 0 0 $ \ n -> "." ++ n ++ " td.progress"
    _ <- p # JQuery.setHtml $ js $ "<b align=\"center\">" ++
                    show (length result') ++ " test(s), "++
                    show (length [ () | Just _ <- result' ]) ++ " passed / " ++
                    show (length [ () | Nothing <- result' ]) ++ " failed " ++
                    "</b>"
    return ()

  let ts :: [Timings [Double]] = [ fmap (:[]) t | Just t <- result' ]
  case teStyle doc of
    TestWithTiming | length ts /= 0 -> do
            writeTimings doc 0 0
                $ fmap geometricMean
                $ foldr1 (<>) ts
    _ -> return ()

  return ()




pbName :: Int -> Int -> String
pbName i j = "pb-" ++ show i ++ "-" ++ show j

pbObject :: Int -> Int -> (String -> String) -> JS t JSObject
pbObject i j f = jQuery $ js $ f $ pbName i j

progressVal :: TestEngine -> Int -> Int -> Int -> Int -> IO ()
progressVal doc i j n np = asyncJS (srEngine doc) $ do
  p  <- pbObject i j $ \ n' -> "." ++ n' ++ " .progressbar"
  () <- p # invoke "progressbar" ( "option" :: JSString
                           , "value" :: JSString
                           , js np :: JSNumber)
  p' <- pbObject i j $ \ n' -> "." ++ n' ++ " .count"
  _  <- p' # JQuery.setHtml (cast ("" <> cast (js n) :: JSString))
  return ()

overwriteMessage :: TestEngine -> Int -> Int -> String -> String -> IO ()
overwriteMessage doc i j msg cls = asyncJS (srEngine doc) $ do
  p <- pbObject i j $ \ n -> "." ++ n ++ " td.progress"
  _ <- p # JQuery.setHtml(js msg)
  p # JQuery.addClass(js cls)
  return ()

writeTimings :: TestEngine -> Int -> Int -> Timings Double -> IO ()
writeTimings doc _ _ _ | teStyle doc /= TestWithTiming = return ()
writeTimings doc i j t = asyncJS (srEngine doc) $ do
        pnt 1 (compileTime t)
        pnt 2 (sendTime t)
        pnt 3 (waitTime t)
        return ()
  where
        pnt :: Int -> Double -> JS t ()
        pnt n v = do
                p <- pbObject i j $ \ nd -> "." ++ nd ++ " td.data" ++ show n
                _ <- p # JQuery.setHtml (js $ Numeric.showFFloat (Just 2) v "s")
                return ()

-- -----------------------------------------------------------------------
-- Test Utilities
-- -----------------------------------------------------------------------

-- | Look if two fractional values are almost the same.
deltaEqual :: (Ord a, Fractional a) => a -> a -> Bool
deltaEqual x y = x >= y - delta && x <= y + delta
  where delta = 0.00000000000001

-- | Use to generators with the same seed and size.
--   This is useful for overloaded value generation.
--   Example:
--
-- > sameSeed (numGen :: Gen Double) (numGen :: Gen JSNumber)
--
--   Both generators will produce the same overloaded value that can be casted
--   to the appropriate type.
sameSeed :: Gen a -> Gen b -> Gen (a,b)
sameSeed genA genB = MkGen $ \gen size -> (unGen genA gen size, unGen genB gen size)

-- -----------------------------------------------------------------------
-- Custom Generators
-- -----------------------------------------------------------------------

instance Arbitrary JSNumber where
  arbitrary = numGen

instance Arbitrary JSBool where
  arbitrary = fmap js (arbitrary :: Gen Bool)

instance Arbitrary JSString where
  arbitrary = fmap js (arbitrary :: Gen String)

numGen :: (Num b) => Gen b
numGen = do
  n <- arbitrary :: Gen Integer
  return $ fromIntegral $ (n `Prelude.rem` 10000)

numExprGen :: Num a => Int -> Gen a
numExprGen 0 = numGen
numExprGen n = frequency [(1, numGen), (2, binaryGen)]
  where binaryGen :: Num a => Gen a
        binaryGen = do
          op <- elements [(+),(-),(*)]
          e1 <- numExprGen $ n - 1
          e2 <- numExprGen $ n - 1
          return $ e1 `op` e2

{-
eqExprGen :: (EqB a) => Gen a -> Gen (BooleanOf a)
eqExprGen genA = do
  op <- elements [(==*),(/=*)]
  e1 <- genA
  e2 <- genA
  return $ e1 `op` e2

ordExprGen :: (OrdB a) => Gen a -> Gen (BooleanOf a)
ordExprGen genA = do
  op <- elements [(<=*),(>=*),(<*),(>*)]
  e1 <- genA
  e2 <- genA
  return $ e1 `op` e2
-}

boolGen :: (Boolean b) => Gen b
boolGen = elements [true, false]

boolExprGen :: (Boolean b, b ~ BooleanOf b, EqB b, IfB b) => Int -> Gen b
boolExprGen 0 = boolGen
boolExprGen n = frequency [(1, boolGen), (3, binaryGen), (1, ifGen), (1, unaryGen)]
  where binaryGen :: (Boolean b, b ~ BooleanOf b, EqB b, IfB b) => Gen b
        binaryGen = do
          op <- elements [(&&*),(||*),(==*),(/=*)]
          e1 <- boolExprGen $ n - 1
          e2 <- boolExprGen $ n - 1
          return $ e1 `op` e2
        ifGen :: (Boolean b, b ~ BooleanOf b, EqB b, IfB b) => Gen b
        ifGen = do
          e1 <- boolExprGen $ n - 1
          e2 <- boolExprGen $ n - 1
          e3 <- boolExprGen $ n - 1
          return $ ifB e1 e2 e3
        unaryGen :: (Boolean b, b ~ BooleanOf b, EqB b, IfB b) => Gen b
        unaryGen = do
          e1 <- boolExprGen $ n - 1
          return $ notB e1

-- From http://en.wikipedia.org/wiki/Geometric_mean
geometricMean :: Floating a => [a] -> a
geometricMean xs = exp ((1 / n) * sum (map log xs))
  where n = fromIntegral (length xs)

data ArrayConstructor n
        = NewEmptyArray
        | NewArray [n]
  deriving Show

data Val n = Val n | Undefined
        deriving (Eq, Ord, Show)

data ArrayOp n
        = LookupArray Int               (Val n)        -- n is the expected result
        | InsertArray Int n
        | LengthArray                   Int            -- number of elements
        | ElemsArray                    [Val n]
  deriving Show


genArrayOps :: Arbitrary n => (Int,Int) -> Gen (ArrayConstructor n,[ArrayOp n])
genArrayOps (sz1,sz2) = do
   cons <- oneof
        [ return NewEmptyArray
        , do n <- choose (0,sz1)
             vs <- vector n
             return $ NewArray vs
        ]

   n <- choose (0,sz2)   -- how many operations?

   let next i mp c = do
              rest <- pick' (i - 1) mp
              return (c : rest)

       modifiers i mp =
         [ do k <- choose (-10,100)
              v <- arbitrary
              let mp1 = if k < 0 then mp
                                 else Map.insert k (Val v) mp `Map.union`
                                      Map.fromList [ (k',Undefined) | k' <- [(Map.size mp)..(k-1)]]

              next i mp1 (InsertArray k v)
          ]

       observers i mp =
         [ do ok' :: Bool <- arbitrary
              k <- if (ok' && not (Map.null mp))
                         then elements (Map.keys mp)
                         else choose (-10,100)       -- This *might* hit; see test below
              next i mp (LookupArray k
                            $ (\ v -> case v of
                                       Just n' -> n'
                                       _       -> Undefined)
                            $ Map.lookup k mp)
         , do next i mp (ElemsArray (Map.elems mp))
         , do next i mp (LengthArray (Map.size mp))
         ]

       pick' 0 _  = return []
       pick' 1 mp = oneof (observers 1 mp)
       pick' i mp = oneof (observers i mp ++ modifiers i mp)

   ops <- pick' n (Map.fromList $ zip [0..] (case cons of
                                               NewEmptyArray -> []
                                               NewArray xs -> map Val xs))

   return (cons,ops)

data MapOp k n
        = LookupMap k                   (Val n)  -- n is the expected result
        | InsertMap k n
        | DeleteMap k
        | SizeMap                       Int                 -- number of elements
--        | ElemsMap                      [n]           -- tricky to test
  deriving Show

genMapOps :: (Ord k, Arbitrary k, Arbitrary n) => Int -> Gen [MapOp k n]
genMapOps sz1 = do
   n <- choose (0,sz1)   -- how many operations?

   let next i mp c = do
              rest <- pick' (i - 1) mp
              return (c : rest)

       modifiers i mp =
         [ do k <- arbitrary
              v <- arbitrary
              let mp1 = Map.insert k v mp
              next i mp1 (InsertMap k v)
          ]

       observers i mp =
         [ do ok' :: Bool <- arbitrary
              k <- if (ok' && not (Map.null mp))
                          then elements (Map.keys mp)
                          else arbitrary
              next i mp (LookupMap k
                            $ (\ v -> case v of
                                       Just n' -> Val n'
                                       _       -> Undefined)
                            $ Map.lookup k mp)
--         , do next i mp (ElemsMap (Map.elems mp))
         , do next i mp (SizeMap (Map.size mp))
         ]

       pick' 0 _  = return []
       pick' 1 mp = oneof (observers 1 mp)
       pick' i mp = oneof (observers i mp ++ modifiers i mp)

   ops <- pick' n (Map.empty)

   return ops


newtype SmallNat = SmallNat Int
   deriving (Eq, Ord)

instance SunroofValue SmallNat where
   type ValueOf SmallNat = JSNumber
   js (SmallNat n) = js n

instance Show SmallNat where
   show (SmallNat n) = show n

instance Arbitrary SmallNat where
  arbitrary = sized $ \ n -> fmap (SmallNat . fromInteger) $ choose (0::Integer,max 0 (min (fromIntegral n) 100))

newtype SmallString = SmallString String
   deriving (Eq, Ord)

instance SunroofValue SmallString where
   type ValueOf SmallString = JSString
   js (SmallString n) = js n

instance Show SmallString where
   show (SmallString n) = show n

instance Arbitrary SmallString where
  arbitrary = sized $ \ n -> do
        str <- sequence [ elements "ABC$ "
                        | _ <- [1.. min n 3]
                        ]
        return $ SmallString str


------------------------------------------------------

-- test = quickCheck (forAll (genMapOps 10 :: Gen [MapOp SmallString SmallNat])
--                   $ \ c -> P.label (show c) True)


-- prop :: ArrayConstructor Int -> Property
-- prop c = P.label (show c) $ True

