{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Internal.Shell
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Shell interaction for viewing output.
--
--------------------------------------------------------------------------------

module Payasan.Base.Internal.Shell
  ( 
    payasan_temp_directory
  , default_shell_help

  , outputDirectory
  , showOutputDirectory

  , shellOutABC
  , shellOutLilyPond

  ) where


import Payasan.Base.Internal.CommonSyntax

import Control.Monad
import Control.Exception ( try )

import System.Directory
import System.Environment
import System.FilePath


-- | Environment variable pointing to the GhostScript font
-- directory.
-- 
-- > WUMPUS_GS_FONT_DIR
--
payasan_temp_directory :: String
payasan_temp_directory = "PAYASAN_TEMP_DIR"


default_shell_help :: String
default_shell_help = unlines $ 
    [ "Set the environment variable " ++ payasan_temp_directory
    , "otherwise temporary files will be generated in the current"
    , "working directory."
    ]


outputDirectory :: IO FilePath
outputDirectory = envLookup payasan_temp_directory >>= fn
  where
    fn Nothing      = getCurrentDirectory
    fn (Just ss)    = do { ans <- doesDirectoryExist ss
                         ; if ans then return ss else errk ss
                         }

    errk ss         = error $ unwords [ payasan_temp_directory
                                      , " points to an invalid directory: "
                                      , ss 
                                      ]


showOutputDirectory :: IO ()
showOutputDirectory = outputDirectory >>= putStrLn

envLookup :: String -> IO (Maybe String)
envLookup name = liftM fn $ try $ getEnv name
  where
    fn :: Either IOError String -> Maybe String
    fn (Left _)  = Nothing
    fn (Right a) = Just a


shellOutABC :: GlobalRenderInfo -> String -> IO ()
shellOutABC info abc = 
    do { root <- outputDirectory
       ; let outfile = root </> global_temp_abc_file info
       ; writeFile outfile abc
       ; return ()
       }


shellOutLilyPond :: GlobalRenderInfo -> String -> IO ()
shellOutLilyPond info ly = 
    do { root <- outputDirectory
       ; let outfile = root </> global_temp_ly_file info
       ; writeFile outfile ly
       ; return ()
       }


