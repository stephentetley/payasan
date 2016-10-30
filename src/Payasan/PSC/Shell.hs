{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Shell
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Shell interaction for viewing output.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Shell
  ( 
    payasan_temp_directory
  , default_shell_help

  , outputDirectory
  , showOutputDirectory

  , ShellInfo(..)
  , default_shell_info
  , shellOutABC
  , shellOutLilyPond
  , shellOutCsound

  ) where



import Control.Monad
import Control.Exception ( try )

import Data.Data
import qualified Data.Text              as TEXT
import qualified Data.Text.IO           as TEXT

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


data ShellInfo = ShellInfo
    { shell_temp_abc_file           :: !String
    , shell_temp_ly_file            :: !String
    , shell_temp_csound_file        :: !String
    , shell_csound_notelist_hole    :: !String
    , shell_pathto_csound_template  :: !FilePath
    }
  deriving (Data,Eq,Show,Typeable)


default_shell_info :: ShellInfo
default_shell_info = ShellInfo
    { shell_temp_abc_file           = "abc_output.abc"
    , shell_temp_ly_file            = "output.ly"
    , shell_temp_csound_file        = "output.csd"
    , shell_csound_notelist_hole    = "[|notelist|]"
    , shell_pathto_csound_template  = ""
    }

shellOutABC :: ShellInfo -> String -> IO ()
shellOutABC info abc = 
    do { root <- outputDirectory
       ; let outfile = root </> shell_temp_abc_file info
       ; writeFile outfile abc
       ; return ()
       }


shellOutLilyPond :: ShellInfo -> String -> IO ()
shellOutLilyPond info ly = 
    do { root <- outputDirectory
       ; let outfile    = root </> shell_temp_ly_file info
       ; writeFile outfile ly
       ; return ()
       }


shellOutCsound :: ShellInfo -> String -> IO ()
shellOutCsound info notes = 
    do { 
       ; let tpath      = shell_pathto_csound_template info
       ; texists        <- doesFileExist tpath
       ; case texists of 
            True -> do { template <- TEXT.readFile tpath
                       ; let txt = csoundInsertNotes info notes template
                       ; root <- outputDirectory
                       ; let outfile = root </> shell_temp_csound_file info
                       ; TEXT.writeFile outfile txt
                       }
            False -> error $ "Csound - template csd file missing"
       }

       


csoundInsertNotes :: ShellInfo -> String -> TEXT.Text -> TEXT.Text
csoundInsertNotes info sco = 
    TEXT.replace (TEXT.pack $ shell_csound_notelist_hole info) (TEXT.pack sco)