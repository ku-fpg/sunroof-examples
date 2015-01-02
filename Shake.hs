{-# LANGUAGE CPP #-}
module Main (main) where

import Development.Shake as Shake
import System.Directory
import System.Environment
import Development.Shake.FilePath
import Data.Char
import Data.List

#if MIN_VERSION_base(4,8,0)
import Prelude hiding ((*>))
#endif

-- local repos to use; later look at the srcs to figure out what the external repo does
repos :: [String]
repos = [ "../sunroof-compiler"  -- the sunroof compiler
        , "../sunroof-server"
        ]

findExecs :: [String] -> [(String,Exec)]
findExecs (xs:xss) = case words xs of
        ["Executable",nm] ->
                let loop (xs':xss') o
                        | all isSpace xs'    = loop xss' o
                        | isSpace (head xs') = case words xs' of
                                                ("Main-is:":ns) -> loop xss (o { the_main = unwords ns })
                                                ("Hs-Source-Dirs:":ns) -> loop xss' (o { the_dir = unwords ns })
                                                ("Ghc-Options:":ns) -> loop xss' (o { the_opts = unwords ns })
                                                _ -> loop xss' o
                        | otherwise = (nm,o) : findExecs (xs':xss')
                    loop [] o = (nm,o) : []
                in loop xss (Exec "" "" "")
        _ -> findExecs xss
findExecs [] = []

data Exec = Exec
        { the_main :: String
        , the_dir  :: String
        , the_opts :: String
        }
        deriving Show

main :: IO ()
main = do
        args <- getArgs
        execs <- readFile "sunroof-examples.cabal" >>= return . findExecs . lines
        main2 args execs

main2 :: [String] -> [(String, Exec)] -> IO ()
main2 [] execs = do
        putStrLn "usage: Shake [all] exec_1 exec_2 ..."
        putStrLn ""
        putStrLn "to clean, use cabal clean"
        putStrLn ""
        putStrLn "Executables"
        putStrLn "-----------"
        putStr $ unlines
                [ "Executable " ++ nm ++ " (" ++ the_main exec ++ " in " ++ the_dir exec ++ " with " ++ the_opts exec ++ ")"
                | (nm,exec) <- execs
                ]

-- to clean, use cabal clean

main2 xs execs = shake (shakeOptions) $ do
        let autogen = "inplace-autogen/Paths_sunroof_examples.hs"
        let targets =
             [ "dist" </> "build" </> nm </> nm
             | (nm,_ex) <- execs
             , xs == ["all" ]|| nm `elem` xs
             ]
        want [autogen]

        want targets

        autogen *> \ out ->  do
                pwd <- liftIO $ getCurrentDirectory
                writeFile' out $ unlines
                  [ "module Paths_sunroof_examples where"
                  , "getDataDir :: IO String"
                  , "getDataDir = return " ++ show pwd
                  ]

        targets **> \ out -> do
                liftIO $ print out
                let nm = takeFileName out
                liftIO $ print nm
                let Just exec = lookup nm execs
                liftIO $ print exec

                need [ the_dir exec </> the_main exec]

                sequence_
                   [ do files <- getDirectoryFiles repo ["*.cabal"]
                        liftIO $ print files
                        need $ map (repo </>) files
                        files' <- getDirectoryFiles repo ["//*.hs"]
                        liftIO $ print files'
                        need $ map (repo </>) files'
                   | repo <- repos
                   ]

                liftIO $ putStrLn $ "Building: " ++ out
                -- compile inside the build dir
                let cache = takeDirectory out </> "cache"
                systemCwd "."
                          "ghc"
                          ["--make",the_dir exec </> the_main exec, the_opts exec,
                           "-hidir",cache,"-odir",cache,
                           "-dcore-lint",
                           "-o", out,
                           "-i" ++ concat (intersperse  ":"  (the_dir exec : "inplace-autogen" : repos))
                           ]
