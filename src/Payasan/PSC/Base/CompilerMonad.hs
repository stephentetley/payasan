{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.PSC.Base.CompilerMonad
-- Copyright   :  (c) Stephen Tetley 2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Compiler monad.
--
--------------------------------------------------------------------------------

module Payasan.PSC.Base.CompilerMonad
  ( 
    
    CM
  , CMEnv(..)
  , ErrMsg
  
  , runCM
  , prompt
  
  , localCE
  , askCE
  , asksCE

  , localUE
  , askUE
  , asksUE
 
  , rewrite
 
  , payasan_TEMP_DIR_VARIABLE
  , TempDirLoc(..)
  , default_temp_dir_location
  
  , getTempDirectory
  
  , getEnvCM
  , localWorkingDirectory
  , localWorkingDirectory_byEnv
  
  , printWorkingDirectory
  
  ) where


import Payasan.PSC.Base.RewriteMonad

import Control.Monad.IO.Class
import Control.Exception ( try )


import Data.Data

import System.Directory
import System.Environment


type ErrMsg = String


-- | The env *must avoid* being backend specific, use the 
-- user_env for backend specific configuration info.
--
-- We expect more entries in this record... 
-- e.g. debug level.
--
data CMEnv = CMEnv 
    { cm_working_directory      :: !FilePath
    , cm_outfile_name           :: !String
    }
    
    
zero_CMEnv :: IO CMEnv
zero_CMEnv = 
    do { cwd <- getCurrentDirectory
       ; return $ CMEnv { cm_working_directory = cwd
                        , cm_outfile_name      = "temp.out"
                        }
       }


-- Note Parsec uses a smaller dictionary to make a bigger one.
-- We could do likewise to get working directory from an env var 
-- or FilePath.

       
  
-- | Env (* 2) + Error + IO Monad.
--
-- As we are already in IO, should we log to stdout anyway?
-- It would save the bother of extending the monad with a Writer.
--
newtype CM ue a = CM { getCM :: CMEnv -> ue -> IO (Either ErrMsg a) }


instance Functor (CM ue) where
  fmap f ma = CM $ \q r -> getCM ma q r >>= \ans -> case ans of
        Left err -> return (Left err) 
        Right a -> return (Right $ f a) 
        
instance Applicative (CM ue) where
  pure a    = CM $ \_ _ -> return (Right a)
  mf <*> ma = CM $ \q r -> getCM mf q r >>= \ans -> case ans of 
        Left err -> return (Left err)
        Right f -> getCM ma q r >>= \ans2 -> case ans2 of
            Left err -> return (Left err)
            Right a -> return (Right $ f a)
            

instance Monad (CM ue) where
  return = pure
  ma >>= k = CM $ \q r -> getCM ma q r >>= \ans -> case ans of
        Left err -> return (Left err)
        Right a -> getCM (k a) q r
            

-- @local@ etc.            
localCE :: (CMEnv -> CMEnv) -> CM ue a -> CM ue a
localCE f ma = CM $ \q r -> getCM ma (f q) r

askCE :: CM ue CMEnv
askCE = CM $ \q _ -> return (Right q)

asksCE :: (CMEnv -> a) -> CM ue a
asksCE f = CM $ \q _ -> return (Right $ f q)



localUE :: (ue -> ue) -> CM ue a -> CM ue a
localUE f ma = CM $ \q r -> getCM ma q (f r)

askUE :: CM ue ue
askUE = CM $ \_ r -> return (Right r)

asksUE :: (ue -> a) -> CM ue a
asksUE f = CM $ \_ r -> return (Right $ f r)


throwErr :: ErrMsg -> CM ue a
throwErr msg = CM $ \_ _ -> return (Left msg)


-- | @guard@ might be the wrong name
guard :: CM ue a -> (ErrMsg -> CM ue a) -> CM ue a
guard ma handler = CM $ \q r -> getCM ma q r >>= \a -> case a of
    Left err  -> getCM (handler err) q r
    Right ans -> return $ Right ans

instance MonadIO (CM ue) where
  liftIO ma = CM $ \_ _ -> ma >>= \a -> return (Right a)


  

runCM :: ue -> CM ue a -> IO (Either ErrMsg a)
runCM ue ma = 
    do { ce <- zero_CMEnv
       ; getCM ma ce ue
       }

prompt :: ue -> CM ue a -> IO a
prompt ue ma = runCM ue ma >>= \ans -> case ans of
    Left err -> error err
    Right a -> return a


--------------------------------------------------------------------------------
-- Run rewrites

rewrite :: Rewrite env st a -> env -> st -> CM ue a
rewrite ma env st = CM $ \_ _ -> return (evalRewrite ma env st)


--------------------------------------------------------------------------------
-- Working with temp files

-- A "compiler" might want to run a shell command to invoke 
-- lilypond or abcm2ps and generate a pdf or PostScript file 
-- as the final result (the abc or ly file will be considered
-- a temp). Therefore working with temp files is one of the 
-- services the CompilerMonad should provide help with.


-- | Environment variable pointing to the temp dir.
-- 
-- > PAYASAN_TEMP_DIR
--
-- Obviously a user can set the variable (or even change
-- it's name to something else in compilers that use it).
--
payasan_TEMP_DIR_VARIABLE :: String
payasan_TEMP_DIR_VARIABLE = "PAYASAN_TEMP_DIR"


data TempDirLoc = EnvVarPointer  !String
                | NamedDirectory !FilePath
  deriving (Data,Eq,Show,Typeable)

  
default_temp_dir_location :: TempDirLoc
default_temp_dir_location = EnvVarPointer payasan_TEMP_DIR_VARIABLE
  

-- | TODO - should print a failure warning, before defaulting to cwd.
--
getTempDirectory :: TempDirLoc -> CM ue FilePath
getTempDirectory loc = 
    guard (step1 loc) (\_ -> liftIO getCurrentDirectory)
  where
    step1 (EnvVarPointer name)  = getEnvCM name >>= getDir
    step1 (NamedDirectory path) = getDir path
       
    getDir dir  = do { ans <- liftIO $ doesDirectoryExist dir
                     ; if ans then return dir
                              else throwErr $ "Directory does not exist: " ++ dir
                     }
 
  
       
-- | CM version of @getEnv@.
--
-- Variable lookup failure is caught in the CM monad.
--
getEnvCM :: String -> CM ue String
getEnvCM name = CM $ \_ _ -> 
    do { a <- try (getEnv name); return (fn a) }
  where
    fn :: Either IOError String -> Either ErrMsg String
    fn (Left _)  = Left $ "Could not deref environment variable: " ++ name
    fn (Right a) = Right a
    


-- Probably need a version of try (bracket/handle...)

localWorkingDirectory :: FilePath -> CM ue a -> CM ue a
localWorkingDirectory loc ma = 
    do { ans <- liftIO $ doesDirectoryExist loc
       ; if ans then localCE (\s -> s { cm_working_directory = loc }) ma
                else throwErr $ "Directory does not exist: " ++ loc
       }
       
       
localWorkingDirectory_byEnv :: String -> CM ue a -> CM ue a
localWorkingDirectory_byEnv name ma = 
    do { loc <- getEnvCM name
       ; ans <- liftIO $ doesDirectoryExist loc
       ; if ans then localCE (\s -> s { cm_working_directory = loc }) ma
                else throwErr $ "Directory does not exist: " ++ loc
       }
    
printWorkingDirectory :: CM ue ()
printWorkingDirectory = 
    asksCE cm_working_directory >>= \cwd -> liftIO (putStrLn cwd)

    
-- Functions prefixed with print should always print to stdout
    
-- For logging, perhaps something like Writer @tell@ with error 
-- level:
-- > tell :: ErrLevel -> String -> CM ()


    

