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
  , atStart
  , atEnd
  , makeLoc
  , unwindLoc
  , change
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
unwindLoc (Loc info _ EmptyPhrase)         = 
    Phrase { phrase_header = info, phrase_bars =  [] }

unwindLoc (Loc info xs (Ctx yl ys))  = 
    Phrase { phrase_header = info, phrase_bars = fn ys $ unwindY yl : xs }
  where
    fn (a:as) bs = fn as (a:bs)
    fn []     bs = bs

change :: Element pch drn anno -> Loc pch drn anno -> Loc pch drn anno
change _ (Loc info xs EmptyPhrase)  = Loc info xs EmptyPhrase
change a (Loc info xs (Ctx yl ys))  = Loc info xs $ Ctx (changeY a yl) ys


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
unwindY (LocBar _ EmptyBar)         = Bar { bar_groups = [] }
unwindY (LocBar xs (CtxBar zl zs))  = 
    Bar { bar_groups = fn zs $ unwindZ zl : xs }
  where
    fn (a:as) bs = fn as (a:bs)
    fn []     bs = bs



changeY :: Element pch drn anno -> LocBar pch drn anno -> LocBar pch drn anno
changeY _ (LocBar xs EmptyBar)          = LocBar xs EmptyBar
changeY a (LocBar xs (CtxBar z1 zs))    = LocBar xs $ CtxBar (changeZ a z1) zs

--------------------------------------------------------------------------------
-- Note Group = Z

data LocNG pch drn anno = LocNG
    { ng_at             :: NoteGroup pch drn anno
    , ng_ctx            :: CtxNG pch drn anno
    }
  deriving (Data,Eq,Show,Typeable)

-- No recursion in NoteGroup
type CtxNG pch drn anno = [Element pch drn anno]



rightZ :: LocNG pch drn anno -> LocNG pch drn anno
rightZ (LocNG (Atom a)             ctx) = LocNG (Atom a) ctx
rightZ (LocNG (Tuplet spec (a:as)) ctx) = LocNG (Tuplet spec as) (a:ctx)
rightZ (LocNG (Tuplet spec [])     ctx) = LocNG (Tuplet spec []) ctx

leftZ :: LocNG pch drn anno -> LocNG pch drn anno
leftZ (LocNG (Atom a)         ctx)  = LocNG (Atom a) ctx
leftZ (LocNG (Tuplet spec xs) ctx)  = case ctx of
    (y:ys) -> LocNG (Tuplet spec (y:xs)) ys
    []     -> LocNG (Tuplet spec  xs) ctx


atEndZ :: LocNG pch drn anno -> Bool
atEndZ (LocNG (Atom _)      _)  = True
atEndZ (LocNG (Tuplet _ xs) _)  = null xs

atStartZ :: LocNG pch drn anno -> Bool
atStartZ (LocNG _ xs)           = null xs


makeZ :: NoteGroup pch drn anno -> LocNG pch drn anno
makeZ a = LocNG { ng_at = a, ng_ctx = [] }

unwindZ :: LocNG pch drn anno -> NoteGroup pch drn anno
unwindZ (LocNG (Atom a)         _)  = Atom a
unwindZ (LocNG (Tuplet spec xs) ys) = Tuplet spec (fn ys xs)
  where
    fn (a:as) bs = fn as (a:bs)
    fn []     bs = bs




changeZ :: Element pch drn anno -> LocNG pch drn anno -> LocNG pch drn anno
changeZ a (LocNG (Atom _)             ctx)  = LocNG (Atom a) ctx
changeZ a (LocNG (Tuplet spec (_:xs)) ctx)  = LocNG (Tuplet spec (a:xs)) ctx
changeZ _ (LocNG (Tuplet spec [])     ctx)  = LocNG (Tuplet spec []) ctx
    


