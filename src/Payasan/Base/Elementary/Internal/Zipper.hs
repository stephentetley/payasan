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
  , forward
  , backward
  , makeLoc
  , unwindLoc
  , change
  , remaining
  , consumed

  )  where


import Payasan.Base.Elementary.Internal.Syntax
import Payasan.Base.Internal.CommonSyntax

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


-- Need an end sentinel cf. parsing.
-- Note this is getting more like parsing than zipping...

-- put ctx before inp - ctx is the left, inp is the right


-- A modular implementation (zip bars, descend and zip 
-- note-groups, descend and zip tuplets) is problematic due 
-- to counting errors.
--
-- At the end of a bar and at the start of a new bar become 
-- separate states so counting (iterating) with one operation 
-- e.g. @forward@ needs non-intuitive counting handling.
--

data Loc pch drn anno = Loc 
    { loc_info       :: !SectionInfo 
    , loc_stk        :: Stk (Bar pch drn anno)
    , loc_input      :: Inp pch drn anno
    }
  deriving (Data,Eq,Show,Typeable)


data Inp pch drn anno = Nil
                      | Inp (LocBar pch drn anno) [Bar pch drn anno]
  deriving (Data,Eq,Show,Typeable)



forward :: Loc pch drn anno -> Loc pch drn anno
forward loc = maybe loc id $ fwrdLoc loc

backward :: Loc pch drn anno -> Loc pch drn anno
backward loc = maybe loc id $ bwrdLoc loc

-- Need a special test for end-of-bar (after initial forward)
fwrdLoc :: Loc pch drn anno -> Maybe (Loc pch drn anno)
fwrdLoc (Loc _    _   Nil)          = Nothing
fwrdLoc (Loc info stk (Inp bl as))  = 
    let bl2 = fwrdBar bl in
    if atEndBar bl2 then Just $ Loc info (unwindLocBar bl : stk) (makeInp as)
                    else Just $ Loc info stk (Inp bl2 as)


-- If at-the-start of bar before doing any back-tracking we 
-- want to pop the stack.
-- Plus when popping stack we want to go to the right end...
--
bwrdLoc :: Loc pch drn anno -> Maybe (Loc pch drn anno)
bwrdLoc (Loc _    [] Nil)           = Nothing
bwrdLoc (Loc info [] (Inp bl as))   = 
    if atStartBar bl then Nothing
                     else let bl2 = bwrdBar bl 
                          in Just $ Loc info [] (Inp bl2 as)

bwrdLoc (Loc info stk@(s:ss) inp)   = case inp of
    Inp bl as -> 
        if atStartBar bl then let inps = s : unwindLocBar bl : as
                              in Just $ Loc info ss (rightmost1CurrentBar $ makeInp inps)
                         else let bl2 = bwrdBar bl
                              in Just $ Loc info stk (Inp bl2 as)
    Nil -> Just $ Loc info ss (makeInp [s])

rightmost1CurrentBar :: Inp pch drn anno -> Inp pch drn anno
rightmost1CurrentBar Nil            = Nil
rightmost1CurrentBar (Inp bl xs)    = Inp (rightmost1Bar bl) xs


makeLoc :: Phrase pch drn anno -> Loc pch drn anno
makeLoc (Phrase info bs)        = 
    Loc { loc_info = info, loc_stk = stk_empty, loc_input = makeInp bs }

makeInp :: [Bar pch drn anno] -> Inp pch drn anno
makeInp []                   = Nil
makeInp (b:bs)               = Inp (makeLocBar b) bs


unwindLoc :: Loc pch drn anno -> Phrase pch drn anno
unwindLoc (Loc info stk Nil)            = 
    Phrase { phrase_header = info, phrase_bars = unstack stk [] }

unwindLoc (Loc info stk (Inp bl as))    = 
    let inps = unwindLocBar bl : as
    in Phrase { phrase_header = info, phrase_bars = unstack stk inps }


change :: Element pch drn anno -> Loc pch drn anno -> Loc pch drn anno
change _ (Loc info stk Nil)             = Loc info stk Nil
change a (Loc info stk (Inp bl as))     = Loc info stk $ Inp (changeBar a bl) as



remaining :: Loc pch drn anno -> Phrase pch drn anno
remaining (Loc info _ Nil)          = 
    Phrase { phrase_header = info, phrase_bars = [] }

remaining (Loc info _ (Inp bl as))  = 
    let inps = case remainingBar bl of { Nothing -> as; Just b -> b:as }
    in Phrase { phrase_header = info, phrase_bars = inps }


consumed :: Loc pch drn anno -> Phrase pch drn anno
consumed (Loc info stk Nil)          = 
    Phrase { phrase_header = info, phrase_bars = unstack stk []  }

consumed (Loc info stk (Inp bl _))   = 
    let inps = case consumedBar bl of { Nothing -> []; Just b -> [b] }
    in Phrase { phrase_header = info, phrase_bars = unstack stk inps }



--------------------------------------------------------------------------------
-- Bar = Y

-- TODO - the idea of being RIGHT_OF an atom is wrong, should only be 
-- RIGHT_OF at the very end when all input is consumed.
--

 

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


fwrdBar :: LocBar pch drn anno -> LocBar pch drn anno
fwrdBar (LocBar stk BNil)               = LocBar stk BNil

fwrdBar (LocBar stk (InpAtom a  as))    = LocBar (Atom a : stk) (makeBarInp as)

fwrdBar (LocBar stk (InpTupl tl as))    = 
    case fwrdTupl tl of
        Nothing -> LocBar (unwindTupl tl : stk) (makeBarInp as)
        Just tl2 -> LocBar stk (InpTupl tl2 as)


bwrdBar :: LocBar pch drn anno -> LocBar pch drn anno
bwrdBar (LocBar [] inp)        = case inp of
    InpTupl tl as -> case bwrdTupl tl of
        Nothing -> LocBar [] inp
        Just tl2 -> LocBar [] (InpTupl tl2 as)
    _ -> LocBar [] inp

bwrdBar (LocBar stk@(s:ss) inp) = case inp of
    InpTupl tl as -> case bwrdTupl tl of
        Nothing -> let inps = s : unwindTupl tl : as
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

rightmost1Bar loc                  = rightmost1Bar $ fwrdBar loc


makeLocBar :: Bar pch drn anno -> LocBar pch drn anno
makeLocBar (Bar {bar_groups = xs}) = 
    LocBar { bar_stk = stk_empty, bar_input = makeBarInp xs }

makeBarInp :: [NoteGroup pch drn anno] -> BarInp pch drn anno
makeBarInp []                   = BNil
makeBarInp (Atom e:xs)          = InpAtom e xs
makeBarInp (Tuplet spec es:xs)  = InpTupl (makeLocTupl spec es) xs
                   

unwindLocBar :: LocBar pch drn anno -> Bar pch drn anno
unwindLocBar (LocBar stk BNil)            = 
    Bar { bar_groups = unstack stk [] }

unwindLocBar (LocBar stk (InpAtom a as))  = 
    Bar { bar_groups = unstack stk (Atom a:as) }

unwindLocBar (LocBar stk (InpTupl tl as)) = 
    Bar { bar_groups = unstack stk (unwindTupl tl : as) }


changeBar :: Element pch drn anno -> LocBar pch drn anno -> LocBar pch drn anno
changeBar _ (LocBar stk BNil)             = LocBar stk BNil
changeBar a (LocBar stk (InpAtom _ as))   = LocBar stk $ InpAtom a as
changeBar a (LocBar stk (InpTupl tl as))  = 
    LocBar stk $ InpTupl (changeTupl1 a tl) as


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
-- Note Group = Z

-- TODO - this does not tell us if we have /consumed/ an atom

-- case distinction on Loc for Atom or Tuplet...

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


unwindTupl :: LocTupl pch drn anno -> NoteGroup pch drn anno
unwindTupl (LocTupl spec stk inp)       = Tuplet spec (unstack stk inp)


changeTupl1 :: Element pch drn anno 
            -> LocTupl pch drn anno -> LocTupl pch drn anno
changeTupl1 _ (LocTupl spec stk [])     = LocTupl spec stk []
changeTupl1 a (LocTupl spec stk (_:as)) = LocTupl spec stk (a:as)
    

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


