{-# LANGUAGE DeriveDataTypeable         #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Score.Elementary.Internal.Zipper
-- Copyright   :  (c) Stephen Tetley 2015-2016
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Zipper for the elementary notelist.
--
--------------------------------------------------------------------------------

module Payasan.Score.Elementary.Internal.Zipper
  (
    Loc
  , makeLoc
  , fromLoc
  , forward
  , backward
  , gotoFront
  , gotoPosition
  , change
  , adjust
  , atLoc
  , remaining
  , consumed

  )  where


import Payasan.Score.Elementary.Internal.Syntax

import Payasan.Score.Analysis.Common
import Payasan.PSC.Base.SyntaxCommon

import Data.Data


--------------------------------------------------------------------------------
-- Zipper


-- Parsed input is put on a stack

type Stk a = [a]

stk_empty :: [a]
stk_empty = []

unstack :: Stk a -> [a] -> [a]
unstack []     ys = ys
unstack (x:xs) ys = unstack xs (x:ys)



data Loc pch drn anno = Loc 
    { loc_info       :: !SectionInfo 
    , loc_stk        :: Stk (Bar pch drn anno)
    , loc_input      :: Inp pch drn anno
    }
  deriving (Data,Eq,Show,Typeable)


data Inp pch drn anno = Nil
                      | Inp (LocBar pch drn anno) [Bar pch drn anno]
  deriving (Data,Eq,Show,Typeable)




makeLoc :: Section pch drn anno -> Loc pch drn anno
makeLoc (Section info bs)        = 
    Loc { loc_info = info, loc_stk = stk_empty, loc_input = makeInp bs }

makeInp :: [Bar pch drn anno] -> Inp pch drn anno
makeInp []                   = Nil
makeInp (b:bs)               = Inp (makeLocBar b) bs


fromLoc :: Loc pch drn anno -> Section pch drn anno
fromLoc (Loc info stk Nil)          = 
    Section { section_info = info, section_bars = unstack stk [] }

fromLoc (Loc info stk (Inp bl as))  = 
    let inps = fromLocBar bl : as
    in Section { section_info = info, section_bars = unstack stk inps }



forward :: Loc pch drn anno -> Loc pch drn anno
forward loc = maybe loc id $ fwrdLoc loc

backward :: Loc pch drn anno -> Loc pch drn anno
backward loc = maybe loc id $ bwrdLoc loc

-- API question - is there a case for an decons function?
-- eg. decons :: Loc -> (Maybe Element, Loc)
--

-- Need a special test for end-of-bar (after initial forward)
fwrdLoc :: Loc pch drn anno -> Maybe (Loc pch drn anno)
fwrdLoc (Loc _    _   Nil)          = Nothing
fwrdLoc (Loc info stk (Inp bl as))  = 
    let bl2 = fwrdLocBar bl in
    if atEndBar bl2 then Just $ Loc info (fromLocBar bl : stk) (makeInp as)
                    else Just $ Loc info stk (Inp bl2 as)


gotoFront :: Loc pch drn anno -> Loc pch drn anno
gotoFront = makeLoc . fromLoc

-- Useful to set location by Position

gotoPosition :: Position -> Loc pch drn anno -> Loc pch drn anno
gotoPosition (Position { position_bar = b, position_index = ix }) = step1 b . gotoFront
  where
    step1 n loc | n > 1     = step1 (n-1) $ forwardBar loc
                | otherwise = step2 ix loc

    step2 n loc | n > 1     = step1 (n-1) $ forward loc
                | otherwise = loc


-- DESIGN NOTE - Do not expose forwardBar - only use for 
-- gotoPosition. i.e should the goto 
--
-- fowardBar (and backwardBar) would have confusing semantics 
-- if exposed. Should they go forward just to start or also 
-- advance forward relative to current position within bar.
-- 
-- For gotoPosition forward bar should just go forward to start 
-- of next bar.
--
forwardBar :: Loc pch drn anno -> Loc pch drn anno
forwardBar loc = maybe loc id $ nextBar loc

nextBar :: Loc pch drn anno -> Maybe (Loc pch drn anno)
nextBar (Loc _    _   Nil)          = Nothing
nextBar (Loc info stk (Inp bl as))  = 
    Just $ Loc info (fromLocBar bl : stk) (makeInp as)



-- If at-the-start of bar before doing any back-tracking we 
-- want to pop the stack.
-- Plus when popping stack we want to go to the right end...
--
bwrdLoc :: Loc pch drn anno -> Maybe (Loc pch drn anno)
bwrdLoc (Loc _    [] Nil)           = Nothing
bwrdLoc (Loc info [] (Inp bl as))   = 
    if atStartBar bl then Nothing
                     else let bl2 = bwrdLocBar bl 
                          in Just $ Loc info [] (Inp bl2 as)

bwrdLoc (Loc info stk@(s:ss) inp)   = case inp of
    Inp bl as -> 
        if atStartBar bl then let inps = s : fromLocBar bl : as
                              in Just $ Loc info ss (rightmost1CurrentBar $ makeInp inps)
                         else let bl2 = bwrdLocBar bl
                              in Just $ Loc info stk (Inp bl2 as)
    Nil -> Just $ Loc info ss (makeInp [s])



rightmost1CurrentBar :: Inp pch drn anno -> Inp pch drn anno
rightmost1CurrentBar Nil            = Nil
rightmost1CurrentBar (Inp bl xs)    = Inp (rightmost1Bar bl) xs




change :: Element pch drn anno -> Loc pch drn anno -> Loc pch drn anno
change _ (Loc info stk Nil)             = Loc info stk Nil
change a (Loc info stk (Inp bl as))     = Loc info stk $ Inp (changeBar a bl) as

adjust :: (Element pch drn anno -> Element pch drn anno) 
       -> Loc pch drn anno -> Loc pch drn anno
adjust _  (Loc info stk Nil)             = Loc info stk Nil
adjust fn (Loc info stk (Inp bl as))     = Loc info stk $ Inp (adjustBar fn bl) as


atLoc :: Loc pch drn anno -> Maybe (Element pch drn anno)
atLoc (Loc _ _ Nil)                     = Nothing
atLoc (Loc _ _ (Inp bl _))              = atLocBar bl


remaining :: Loc pch drn anno -> Section pch drn anno
remaining (Loc info _ Nil)          = 
    Section { section_info = info, section_bars = [] }

remaining (Loc info _ (Inp bl as))  = 
    let inps = case remainingBar bl of { Nothing -> as; Just b -> b:as }
    in Section { section_info = info, section_bars = inps }


consumed :: Loc pch drn anno -> Section pch drn anno
consumed (Loc info stk Nil)          = 
    Section { section_info = info, section_bars = unstack stk []  }

consumed (Loc info stk (Inp bl _))   = 
    let inps = case consumedBar bl of { Nothing -> []; Just b -> [b] }
    in Section { section_info = info, section_bars = unstack stk inps }



--------------------------------------------------------------------------------
-- Bar
 

data LocBar pch drn anno = LocBar 
    { bar_stk           :: Stk (NoteGroup pch drn anno)
    , bar_input         :: BarInp pch drn anno
    }
  deriving (Data,Eq,Show,Typeable)


-- | Tuplet and Atom are different operationally (next and prev 
-- descend into tuplet), so we represent this in the stream head
--
data BarInp pch drn anno = BNil
                         | InpAtom (Element pch drn anno) [NoteGroup pch drn anno]
                         | InpTupl (LocTupl pch drn anno) [NoteGroup pch drn anno]
  deriving (Data,Eq,Show,Typeable)


atEndBar :: LocBar pch drn anno -> Bool
atEndBar (LocBar _  BNil)               = True
atEndBar _                              = False


atStartBar :: LocBar pch drn anno -> Bool
atStartBar (LocBar stk (InpTupl tl _))  = null stk && atStartTupl tl
atStartBar (LocBar stk _)               = null stk


fwrdLocBar :: LocBar pch drn anno -> LocBar pch drn anno
fwrdLocBar (LocBar stk BNil)            = LocBar stk BNil

fwrdLocBar (LocBar stk (InpAtom a  as)) = LocBar (Atom a : stk) (makeBarInp as)

fwrdLocBar (LocBar stk (InpTupl tl as)) = 
    case fwrdTupl tl of
        Nothing -> LocBar (fromTupl tl : stk) (makeBarInp as)
        Just tl2 -> LocBar stk (InpTupl tl2 as)


bwrdLocBar :: LocBar pch drn anno -> LocBar pch drn anno
bwrdLocBar (LocBar [] inp)          = case inp of
    InpTupl tl as -> case bwrdTupl tl of
        Nothing -> LocBar [] inp
        Just tl2 -> LocBar [] (InpTupl tl2 as)
    _ -> LocBar [] inp

bwrdLocBar (LocBar stk@(s:ss) inp)  = case inp of
    InpTupl tl as -> case bwrdTupl tl of
        Nothing -> let inps = s : fromTupl tl : as
                   in LocBar ss (makeBarInp inps)
        Just tl2 -> LocBar stk (InpTupl tl2 as)
    InpAtom a as ->  
        let inps = (s : Atom a : as) in LocBar ss (makeBarInp inps)
    BNil -> LocBar ss (makeBarInp [s]) 


rightmost1Bar :: LocBar pch drn anno -> LocBar pch drn anno
rightmost1Bar (LocBar []     BNil) = LocBar [] BNil
rightmost1Bar (LocBar (s:ss) BNil) = 
    let inp   = makeBarInp [s]
        right = case inp of { (InpTupl tl _) -> InpTupl (rightmost1Tupl tl) []
                            ; _ -> inp } 
    in LocBar ss right

rightmost1Bar loc                  = rightmost1Bar $ fwrdLocBar loc


makeLocBar :: Bar pch drn anno -> LocBar pch drn anno
makeLocBar (Bar {bar_groups = xs}) = 
    LocBar { bar_stk = stk_empty, bar_input = makeBarInp xs }

makeBarInp :: [NoteGroup pch drn anno] -> BarInp pch drn anno
makeBarInp []                   = BNil
makeBarInp (Atom e:xs)          = InpAtom e xs
makeBarInp (Tuplet spec es:xs)  = InpTupl (makeLocTupl spec es) xs
                   

fromLocBar :: LocBar pch drn anno -> Bar pch drn anno
fromLocBar (LocBar stk BNil)            = 
    Bar { bar_groups = unstack stk [] }

fromLocBar (LocBar stk (InpAtom a as))  = 
    Bar { bar_groups = unstack stk (Atom a:as) }

fromLocBar (LocBar stk (InpTupl tl as)) = 
    Bar { bar_groups = unstack stk (fromTupl tl : as) }


changeBar :: Element pch drn anno -> LocBar pch drn anno -> LocBar pch drn anno
changeBar _ (LocBar stk BNil)             = LocBar stk BNil
changeBar a (LocBar stk (InpAtom _ as))   = LocBar stk $ InpAtom a as
changeBar a (LocBar stk (InpTupl tl as))  = 
    LocBar stk $ InpTupl (changeTupl1 a tl) as


adjustBar :: (Element pch drn anno -> Element pch drn anno) 
          -> LocBar pch drn anno -> LocBar pch drn anno
adjustBar _  (LocBar stk BNil)             = LocBar stk BNil
adjustBar fn (LocBar stk (InpAtom e as))   = LocBar stk $ InpAtom (fn e) as
adjustBar fn (LocBar stk (InpTupl tl as))  = 
    LocBar stk $ InpTupl (adjustTupl1 fn tl) as



atLocBar :: LocBar pch drn anno -> Maybe (Element pch drn anno)
atLocBar (LocBar _ BNil)                  = Nothing
atLocBar (LocBar _ (InpAtom e _))         = Just e
atLocBar (LocBar _ (InpTupl tl _))        = atLocTupl tl



remainingBar :: LocBar pch drn anno -> Maybe (Bar pch drn anno)
remainingBar (LocBar _ BNil)            = Nothing 

remainingBar (LocBar _ (InpAtom a as))  = 
    Just $ Bar { bar_groups = Atom a : as }

remainingBar (LocBar _ (InpTupl tl as)) = 
    let inps = case remainingTupl tl of { Nothing -> as; Just t -> t:as }
    in Just $ Bar { bar_groups = inps }


consumedBar :: LocBar pch drn anno -> Maybe (Bar pch drn anno)
consumedBar (LocBar stk (InpTupl tl _)) = 
    let tcons = case consumedTupl tl of { Nothing -> []; Just t -> [t] }
        elems = unstack stk tcons 
    in if null elems then Nothing else Just $ Bar { bar_groups = elems }

consumedBar (LocBar stk _)              = 
    if null stk then Nothing else Just $ Bar { bar_groups = unstack stk [] }




--------------------------------------------------------------------------------
-- Tuplet



data LocTupl pch drn anno = LocTupl
    { tupl_spec          :: !TupletSpec
    , tupl_stk           :: Stk (Element pch drn anno)
    , tupl_input         :: [Element pch drn anno]
    }
  deriving (Data,Eq,Show,Typeable)


atStartTupl :: LocTupl pch drn anno -> Bool
atStartTupl (LocTupl _ stk _)           = null stk



fwrdTupl :: LocTupl pch drn anno -> Maybe (LocTupl pch drn anno)
fwrdTupl (LocTupl spec stk (a:as))      = Just $ LocTupl spec (a:stk) as
fwrdTupl (LocTupl _    _   [])          = Nothing



bwrdTupl :: LocTupl pch drn anno -> Maybe (LocTupl pch drn anno)
bwrdTupl (LocTupl spec (s:stk) as)      = Just $ LocTupl spec stk (s:as)
bwrdTupl (LocTupl _    []      _)       = Nothing



rightmost1Tupl :: LocTupl pch drn anno -> LocTupl pch drn anno
rightmost1Tupl (LocTupl spec []     [])     = LocTupl spec [] [] 
rightmost1Tupl (LocTupl spec (s:ss) [])     = LocTupl spec ss [s] 
rightmost1Tupl (LocTupl spec stk    (x:xs)) = step stk x xs
  where
    step ac e []        = LocTupl spec ac [e]
    step ac e (z:zs)    = step (e:ac) z zs


makeLocTupl :: TupletSpec -> [Element pch drn anno] -> LocTupl pch drn anno
makeLocTupl spec xs                     = 
    LocTupl { tupl_spec = spec, tupl_stk = stk_empty, tupl_input = xs }


fromTupl :: LocTupl pch drn anno -> NoteGroup pch drn anno
fromTupl (LocTupl spec stk inp)         = Tuplet spec (unstack stk inp)


changeTupl1 :: Element pch drn anno 
            -> LocTupl pch drn anno -> LocTupl pch drn anno
changeTupl1 _ (LocTupl spec stk [])     = LocTupl spec stk []
changeTupl1 a (LocTupl spec stk (_:as)) = LocTupl spec stk (a:as)


adjustTupl1 :: (Element pch drn anno -> Element pch drn anno )
            -> LocTupl pch drn anno -> LocTupl pch drn anno
adjustTupl1 _  (LocTupl spec stk [])     = LocTupl spec stk []
adjustTupl1 fn (LocTupl spec stk (a:as)) = LocTupl spec stk (fn a:as)
    

atLocTupl :: LocTupl pch drn anno -> Maybe (Element pch drn anno)
atLocTupl (LocTupl _ _ [])              = Nothing
atLocTupl (LocTupl _ _ (a:_))           = Just a 


-- TODO - recalc spec
remainingTupl :: LocTupl pch drn anno -> Maybe (NoteGroup pch drn anno)
remainingTupl (LocTupl spec _ inp)      
    | null inp      = Nothing 
    | otherwise     = let spec1 = spec in Just $ Tuplet spec1 inp


-- TODO - recalc spec
consumedTupl :: LocTupl pch drn anno -> Maybe (NoteGroup pch drn anno)
consumedTupl (LocTupl spec stk _) 
    | null stk      = Nothing
    | otherwise     = let spec1 = spec in Just $ Tuplet spec1 (unstack stk [])


