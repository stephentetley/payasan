{-# LANGUAGE ScopedTypeVariables #-}
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
  , ErrMsg
  
  , runCM
  , prompt
  

  , throwErr
  , guard

 
  , rewrite
 
  , getWorkingDirectory1
  
  ) where


import Payasan.PSC.Base.RewriteMonad


import Control.Monad.IO.Class
import Control.Exception ( catch, SomeException )
import System.Directory
import System.Environment


type ErrMsg = String




-- Note Parsec uses a smaller dictionary to make a bigger one.
-- We could do likewise to get working directory from an env var 
-- or FilePath.

       
  
-- | Error + IO Monad.
--
-- As we are already in IO, should we log to stdout anyway?
-- It would save the bother of extending the monad with a Writer.
--
newtype CM a = CM { getCM :: IO (Either ErrMsg a) }


instance Functor CM where
  fmap f ma = CM $ getCM ma >>= \ans -> case ans of
        Left err -> return (Left err) 
        Right a -> return (Right $ f a) 
        
instance Applicative CM where
  pure a    = CM $ return (Right a)
  mf <*> ma = CM $ getCM mf >>= \ans -> case ans of 
        Left err -> return (Left err)
        Right f -> getCM ma >>= \ans2 -> case ans2 of
            Left err -> return (Left err)
            Right a -> return (Right $ f a)
            

instance Monad CM where
  return = pure
  ma >>= k = CM $ getCM ma >>= \ans -> case ans of
        Left err -> return (Left err)
        Right a -> getCM (k a)
            


throwErr :: ErrMsg -> CM a
throwErr msg = CM $ return (Left msg)


-- | @guard@ might be the wrong name
guard :: CM a -> (ErrMsg -> CM a) -> CM a
guard ma handler = CM $ getCM ma >>= \a -> case a of
    Left err  -> getCM (handler err)
    Right ans -> return $ Right ans

instance MonadIO (CM) where
  liftIO ma = CM $ ma >>= \a -> return (Right a)


  

runCM :: CM a -> IO (Either ErrMsg a)
runCM ma = getCM ma

prompt :: CM a -> IO a
prompt ma = runCM ma >>= \ans -> case ans of
    Left err -> error err
    Right a -> return a


--------------------------------------------------------------------------------
-- Run rewrites

rewrite :: Rewrite env st a -> env -> st -> CM a
rewrite ma env st = CM $ return (evalRewrite ma env st)


--------------------------------------------------------------------------------
-- Working with temp files

-- A "compiler" might want to run a shell command to invoke 
-- lilypond or abcm2ps and generate a pdf or PostScript file 
-- as the final result (the abc or ly file will be considered
-- a temp). Therefore working with temp files is one of the 
-- services the CompilerMonad should provide help with.





-- Too much faff just to work with a temp file...


getWorkingDirectory1 :: (Either String FilePath) -> CM FilePath
getWorkingDirectory1 loc = 
    liftIO $ catch (step1 loc) (\(_ :: SomeException) -> getCurrentDirectory)
  where
    step1 :: (Either String FilePath) -> IO FilePath
    step1 (Left name)  = getEnv name >>= getDir
    step1 (Right path) = getDir path
       
    getDir dir  = do { ans <- doesDirectoryExist dir
                     ; if ans then return dir
                              else error $ "Directory does not exist: " ++ dir
                     }
 

