{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.Zipper
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Zipper for the elementary notelist.
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.Zipper
  (
    Loc
  , right
  , left
  , forward
  , backward
  , atStart
  , atEnd
  , makeLoc
  , unwindLoc
  , change
  , contentL
  , contentR
  )

  where


import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Internal.CommonSyntax

import Data.Data

--------------------------------------------------------------------------------
-- Zipper



data Loc pch drn anno = Loc 
    { loc_info       :: !SectionInfo 
    , loc_at         :: [Bar pch drn anno]
    , loc_ctx        :: Ctx pch drn anno
    }
  deriving (Data,Eq,Show,Typeable)

data Ctx pch drn anno = EmptyPhrase 
                      | Ctx (LocBar pch drn anno) [Bar pch drn anno]
  deriving (Data,Eq,Show,Typeable)

-- TODO - right at end of bar not working...
right :: Loc pch drn anno -> Loc pch drn anno
right (Loc info xs      EmptyPhrase)    = Loc info xs EmptyPhrase
right (Loc info xs ctx@(Ctx yl ys)) 
    | atEndY yl = case xs of { [] -> Loc info [] ctx
                             ; (a:as) -> Loc info as (Ctx (makeY a) (unwindY yl : ys))
                             }

    | otherwise = Loc info xs (Ctx (rightY yl) ys)

left :: Loc pch drn anno -> Loc pch drn anno
left (Loc info xs      EmptyPhrase)     = Loc info xs EmptyPhrase
left (Loc info xs ctx@(Ctx yl ys)) 
    | atStartY yl = case ys of { [] -> Loc info xs ctx
                               ; (a:as) -> Loc info (unwindY yl:xs) (Ctx (makeY a) as)
                               }
    | otherwise   = Loc info xs (Ctx (leftY yl) ys)


forward :: Int -> Loc pch drn anno -> Loc pch drn anno
forward = step
  where
    step n loc | n <= 0 = loc
               | otherwise = step (n-1) $ right loc


backward :: Int -> Loc pch drn anno -> Loc pch drn anno
backward = step 
  where
    step n loc | n <= 0 = loc
               | otherwise = step (n-1) $ left loc


atEnd :: Loc pch drn anno -> Bool
atEnd (Loc _ xs EmptyPhrase)        = null xs
atEnd (Loc _ xs (Ctx yl _))         = atEndY yl && null xs

atStart :: Loc pch drn anno -> Bool
atStart (Loc _ _ EmptyPhrase)       = True
atStart (Loc _ _ (Ctx yl ys))       = atStartY yl && null ys


makeLoc :: Phrase pch drn anno -> Loc pch drn anno
makeLoc (Phrase info [])        = 
    Loc {loc_info = info, loc_at = [], loc_ctx = EmptyPhrase }

makeLoc (Phrase info (x:xs))    = 
    Loc { loc_info  = info
        , loc_at    = xs 
        , loc_ctx   = Ctx (makeY x) []
        }

unwindLoc :: Loc pch drn anno -> Phrase pch drn anno
unwindLoc (Loc info _ EmptyPhrase)  = 
    Phrase { phrase_header = info, phrase_bars =  [] }

unwindLoc (Loc info xs (Ctx yl ys)) = 
    Phrase { phrase_header = info, phrase_bars = fn ys $ unwindY yl : xs }
  where
    fn (a:as) bs = fn as (a:bs)
    fn []     bs = bs

change :: Element pch drn anno -> Loc pch drn anno -> Loc pch drn anno
change _ (Loc info xs EmptyPhrase)  = Loc info xs EmptyPhrase
change a (Loc info xs (Ctx yl ys))  = Loc info xs $ Ctx (changeY a yl) ys


contentL :: Loc pch drn anno -> Phrase pch drn anno
contentL (Loc info _ EmptyPhrase)       = 
    Phrase { phrase_header = info, phrase_bars =  [] }

contentL (Loc info _ (Ctx yl ys))       = 
    Phrase { phrase_header = info, phrase_bars = fn ys $ [contentLY yl] }
  where
    fn (a:as) bs = fn as (a:bs)
    fn []     bs = bs

contentR :: Loc pch drn anno -> Phrase pch drn anno
contentR (Loc info _ EmptyPhrase)       = 
    Phrase { phrase_header = info, phrase_bars =  [] }

contentR (Loc info xs (Ctx yl _))       = 
    Phrase { phrase_header = info, phrase_bars = contentRY yl : xs }


--------------------------------------------------------------------------------
-- Bar = Y
 

data LocBar pch drn anno = LocBar 
    { bar_at            :: [NoteGroup pch drn anno]
    , bar_ctx           :: CtxBar pch drn anno
    }
  deriving (Data,Eq,Show,Typeable)

data CtxBar pch drn anno = EmptyBar
                         | CtxBar (LocNG pch drn anno) [NoteGroup pch drn anno]
  deriving (Data,Eq,Show,Typeable)

rightY :: LocBar pch drn anno -> LocBar pch drn anno
rightY (LocBar xs      EmptyBar)        = LocBar xs EmptyBar
rightY (LocBar xs ctx@(CtxBar zl zs))
    | atEndZ zl = case xs of { [] -> LocBar [] ctx
                             ; (a:as) -> LocBar as (CtxBar (makeZ a) (unwindZ zl : zs))
                             }
    | otherwise = LocBar xs (CtxBar (rightZ zl) zs)


leftY :: LocBar pch drn anno -> LocBar pch drn anno
leftY (LocBar xs      EmptyBar)         = LocBar xs EmptyBar
leftY (LocBar xs ctx@(CtxBar zl zs)) 
    | atStartZ zl = case zs of { [] -> LocBar xs ctx
                               ; (a:as) -> LocBar (unwindZ zl:xs) (CtxBar (makeZ a) as)
                               }
    | otherwise   = LocBar xs (CtxBar (leftZ zl) zs)

atEndY :: LocBar pch drn anno -> Bool
atEndY (LocBar xs EmptyBar)         = null xs
atEndY (LocBar xs (CtxBar zl _))    = atEndZ zl && null xs


atStartY :: LocBar pch drn anno -> Bool
atStartY (LocBar _ EmptyBar)        = True
atStartY (LocBar _ (CtxBar zl zs))  = atStartZ zl && null zs


makeY :: Bar pch drn anno -> LocBar pch drn anno
makeY a = case bar_groups a of
  (z:zs) -> LocBar { bar_at = zs
                   , bar_ctx = CtxBar (makeZ z) []
                   }
  []     -> LocBar { bar_at = []
                   , bar_ctx = EmptyBar
                   }

unwindY :: LocBar pch drn anno -> Bar pch drn anno
unwindY (LocBar _  EmptyBar)        = Bar { bar_groups = [] }
unwindY (LocBar xs (CtxBar zl zs))  = 
    Bar { bar_groups = fn zs $ unwindZ zl : xs }
  where
    fn (a:as) bs = fn as (a:bs)
    fn []     bs = bs



changeY :: Element pch drn anno -> LocBar pch drn anno -> LocBar pch drn anno
changeY _ (LocBar xs EmptyBar)          = LocBar xs EmptyBar
changeY a (LocBar xs (CtxBar z1 zs))    = LocBar xs $ CtxBar (changeZ a z1) zs


contentLY :: LocBar pch drn anno -> Bar pch drn anno
contentLY (LocBar _  EmptyBar)        = Bar { bar_groups = [] }
contentLY (LocBar _  (CtxBar zl zs))  = 
    Bar { bar_groups = fn zs $ contentLZ zl }
  where
    fn (a:as) bs = fn as (a:bs)
    fn []     bs = bs

contentRY :: LocBar pch drn anno -> Bar pch drn anno
contentRY (LocBar _  EmptyBar)      = Bar { bar_groups = [] }
contentRY (LocBar xs (CtxBar zl _)) = Bar { bar_groups = contentRZ zl ++ xs}


--------------------------------------------------------------------------------
-- Note Group = Z

-- TODO - this does not tell us if we have /consumed/ an atom

-- case distinction on Loc for Atom or Tuplet...


data LocNG pch drn anno = LocAtom (Element pch drn anno) CtxAtom
                        | LocTupl TupletSpec [Element pch drn anno] (CtxTupl pch drn anno)
  deriving (Data,Eq,Show,Typeable)


-- We need to know if we have consumed an atom
-- 
data CtxAtom = LEFT_OF | RIGHT_OF
  deriving (Data,Eq,Show,Typeable)

type CtxTupl pch drn anno = [Element pch drn anno]



rightZ :: LocNG pch drn anno -> LocNG pch drn anno
rightZ (LocAtom a _)                = LocAtom a RIGHT_OF
rightZ (LocTupl spec (a:as) ctx)    = LocTupl spec as (a:ctx)
rightZ (LocTupl spec []     ctx)    = LocTupl spec [] ctx

leftZ :: LocNG pch drn anno -> LocNG pch drn anno
leftZ (LocAtom a _)                 = LocAtom a LEFT_OF
leftZ (LocTupl spec xs (y:ys))      = LocTupl spec (y:xs) ys
leftZ (LocTupl spec xs [])          = LocTupl spec xs     []


atEndZ :: LocNG pch drn anno -> Bool
atEndZ (LocAtom _ RIGHT_OF)         = True
atEndZ (LocAtom _ LEFT_OF)          = False
atEndZ (LocTupl _ xs _)             = null xs

atStartZ :: LocNG pch drn anno -> Bool
atStartZ (LocAtom _ RIGHT_OF)         = False
atStartZ (LocAtom _ LEFT_OF)          = True
atStartZ (LocTupl _ _ ctx)            = null ctx


makeZ :: NoteGroup pch drn anno -> LocNG pch drn anno
makeZ (Atom a)                        = LocAtom a LEFT_OF
makeZ (Tuplet spec xs)                = LocTupl spec xs []

unwindZ :: LocNG pch drn anno -> NoteGroup pch drn anno
unwindZ (LocAtom a _)                 = Atom a
unwindZ (LocTupl spec xs ys)          = Tuplet spec (fn ys xs)
  where
    fn (a:as) bs = fn as (a:bs)
    fn []     bs = bs




changeZ :: Element pch drn anno -> LocNG pch drn anno -> LocNG pch drn anno
changeZ a (LocAtom _ LEFT_OF)       = LocAtom a LEFT_OF
changeZ _ (LocAtom a RIGHT_OF)      = LocAtom a RIGHT_OF
changeZ a (LocTupl spec (_:xs) ctx) = LocTupl spec (a:xs) ctx
changeZ _ (LocTupl spec []     ctx) = LocTupl spec [] ctx
    


contentLZ :: LocNG pch drn anno -> [NoteGroup pch drn anno]
contentLZ (LocAtom a RIGHT_OF)      = [Atom a]
contentLZ (LocAtom _ LEFT_OF)       = []
contentLZ (LocTupl spec _ ys)       = [Tuplet spec (fn ys [])]
  where
    fn (a:as) bs = fn as (a:bs)
    fn []     bs = bs

contentRZ :: LocNG pch drn anno -> [NoteGroup pch drn anno]
contentRZ (LocAtom _ RIGHT_OF)      = []
contentRZ (LocAtom a LEFT_OF)       = [Atom a]
contentRZ (LocTupl spec xs _)       = [Tuplet spec xs] -- needs remaking
