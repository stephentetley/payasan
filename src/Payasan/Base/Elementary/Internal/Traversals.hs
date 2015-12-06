{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Elementary.Internal.Traversals
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversals of Elementary syntax.
--
--------------------------------------------------------------------------------

module Payasan.Base.Elementary.Internal.Traversals
  (
    Mon 

  , nth
  , take
  , drop
  , takeBars
  , dropBars
  , takeSize
  , dropSize

  , ElemPitchAlgo(..)
  , transformP
  , collectP
  , mapPitch
  , ctxMapPitch   -- TEMP ?
  , foldPitch 


  , ElemDurationAlgo(..)
  , transformD
  , collectD
  , mapDuration
  , foldDuration

  , ElemAnnoAlgo(..)
  , transformA
  , collectA
  , mapAnno
  , foldAnno

  , ElemPitchAnnoAlgo(..)
  , transformPA
  , collectPA
  , mapPitchAnno
  , foldPitchAnno

  , censorPunctuation
  , censorAnno
  , changeSkipToRest

  ) where



import Payasan.Base.Elementary.Internal.RecalcBars
import Payasan.Base.Elementary.Internal.Syntax

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Payasan.Base.Duration

import Data.Foldable (foldlM)
import Data.Maybe

import Prelude hiding (take, drop)
import qualified Prelude as PRE

type Mon st a = Rewrite st a




-- | Do not expose this as it is too general / complex.
--
genCollect :: forall st pch drn anno ac.
              (ac -> Element pch drn anno -> Mon st ac) 
           -> ac 
           -> st
           -> Phrase pch drn anno 
           -> ac
genCollect mf a0 st ph = evalRewrite (phraseC a0 ph) st
  where
    phraseC :: ac -> Phrase pch drn anno -> Mon st ac
    phraseC ac (Phrase info bs) = local info (foldlM barC ac bs)

    barC :: ac -> Bar pch drn anno -> Mon st ac
    barC ac (Bar  cs)           = foldlM noteGroupC ac cs

    noteGroupC :: ac -> NoteGroup pch drn anno -> Mon st ac
    noteGroupC ac (Atom e)      = mf ac e
    noteGroupC ac (Tuplet _ cs) = foldlM mf ac cs


-- | Do not expose this as it is too general / complex.
--
genTransform :: forall st p1 p2 d1 d2 a1 a2. 
                (Element p1 d1 a1 -> Mon st (Element p2 d2 a2))
             -> st
             -> Phrase p1 d1 a1
             -> Phrase p2 d2 a2
genTransform elemT st0 ph = 
    evalRewrite (phraseT ph) st0
  where

    phraseT :: Phrase p1 d1 a1 -> Mon st (Phrase p2 d2 a2) 
    phraseT (Phrase info bs)    = local info (Phrase info <$> mapM barT bs)

    barT :: Bar p1 d1 a1 -> Mon st (Bar p2 d2 a2)
    barT (Bar cs)               = Bar <$> mapM noteGroupT cs

    noteGroupT :: NoteGroup p1 d1 a1 -> Mon st (NoteGroup p2 d2 a2)
    noteGroupT (Atom e)         = Atom <$> elemT e
    noteGroupT (Tuplet spec es) = Tuplet spec <$> mapM elemT es

--------------------------------------------------------------------------------
--

-- Length changing traversals are easier going through flat NoteList
-- 
--

nth :: Int -> StdElemPhrase2 pch anno -> Maybe (StdElemElement2 pch anno)
nth i = onNoteList (\_ xs -> step1 0 xs)
  where 
    step1 _ []                      = Nothing
    step1 n (Atom e:es) | n < i     = step1 (n+1) es
                        | otherwise = Just e

    step1 n (Tuplet _ xs:es)        = case step2 n xs of
        Left n1 -> step1 n1 es
        Right a -> Just a
    
    step2 n []                      = Left n
    step2 n (e:es) | n < i          = step2 (n+1) es
                   | otherwise      = Right e


-- nth suggests take and drop



-- | Tuplet splitting is not properly implemented yet as it 
-- should modify the spec
--
-- TODO - it will be better to define a set of operations that 
-- work on tuplets rather than do ad hoc destructuring here
--  
take :: forall pch anno.
        Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
take i = viaNoteList (\_ xs -> step1 i xs)
  where
    step1 :: Int -> [StdElemNoteGroup2 pch anno]-> [StdElemNoteGroup2 pch anno]
    step1 n _   | n <= 0            = []
    step1 _ []                      = []
    step1 n (Atom e:es)             = (Atom e) : step1 (n-1) es
    step1 n (Tuplet spec xs:es)     = 
        let (n1,ys) = step2 n xs 
            spec2 = spec         
        in Tuplet spec2 ys : step1 n1 es  -- TODO remake spec

    step2 n []                      = (n,[])
    step2 n (e:es) | n <= 0         = (n,[])
                   | otherwise      = let (n1,ys) = step2 (n-1) es
                                      in (n1,e:ys)


drop :: forall pch anno.
        Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
drop i = viaNoteList (\_ xs -> step1 i xs)
  where
    step1 :: Int -> [StdElemNoteGroup2 pch anno]-> [StdElemNoteGroup2 pch anno]
    step1 n xs | n <= 0             = xs
    step1 _ []                      = []
    step1 n (Atom _:es)             = step1 (n-1) es
    step1 n (Tuplet spec xs:es)     = case step2 n xs of
        Left n1 -> step1 n1 es
        Right ys -> let spec2 = spec 
                    in Tuplet spec2 ys : es  -- TODO remake spec

    step2 n []                      = Left n
    step2 n (_:es) | n <= 0         = Right es
                   | otherwise      = step2 (n-1) es
   


-- | TODO - should last element be untied?
--
takeBars :: Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
takeBars i (Phrase info bs) = Phrase info $ PRE.take i bs

dropBars :: Int -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
dropBars i (Phrase info bs) = Phrase info $ PRE.drop i bs


-- This has to be an RDuration as Duration is symbolic 
-- and doesn\'t multiply to arbitrary sizes.
--
takeSize :: forall pch anno.
            RDuration -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
takeSize rd = viaNoteList (\_ xs -> step1 rd xs)
  where
    step1 :: RDuration -> [StdElemNoteGroup2 pch anno]-> [StdElemNoteGroup2 pch anno]
    step1 d _   | d <= 0            = []
    step1 _ []                      = []
    step1 d (Atom e:es)             = 
        let d1 = d - sizeElement e 
        in if d1 < 0 then [] else (Atom e) : step1 d1 es
       

    step1 d (Tuplet spec xs:es)     = 
        let (d1,ys) = step2 d xs 
            spec2   = spec         
        in if null ys then [] else Tuplet spec2 ys : step1 d1 es  -- TODO remake spec


    step2 d []                      = (d,[])
    step2 d (e:es) | d <= 0         = (d,[])
                   | otherwise      = 
        let d1 = d - sizeElement e 
        in if d1 < 0 then (d1,[]) else let (d2,ys) = step2 d1 es in (d2,e:ys)


-- Note - dropSize is really drop-at-least-size as it doesn\'t
-- split too long pivot notes and drops them whole.
--
dropSize :: forall pch anno.
            RDuration -> StdElemPhrase2 pch anno -> StdElemPhrase2 pch anno
dropSize rd = viaNoteList (\_ xs -> step1 rd xs)
  where
    step1 :: RDuration -> [StdElemNoteGroup2 pch anno]-> [StdElemNoteGroup2 pch anno]
    step1 d xs | d <= 0             = xs
    step1 _ []                      = []
    step1 d (Atom e:es)             = 
        let d1 = d - sizeElement e 
        in if d1 <= 0 then es else step1 d1 es


    step1 d (Tuplet spec xs:es)     = case step2 d xs of
        Left d1 -> step1 d1 es
        Right [] -> []
        Right ys -> let spec2 = spec 
                    in Tuplet spec2 ys : es  -- TODO remake spec

    step2 d []                      = Left d
    step2 d (e:es)                  = 
        let d1 = d - sizeElement e
        in if d1 <= 0 then Right es else step2 d1 es
   




--------------------------------------------------------------------------------
--

--
-- Design note - this leaks /shape/, possible to change a rest 
-- to a note or vice-versa.
-- 
-- However, this functionality is for /library writers/ not
-- /top level users/ where it seems that acknowledging the 
-- note-rest distinction is useful.
-- 
-- Also it allows us to use element for Maybe when calculating 
-- contours.
--


data ElemPitchAlgo st pch1 pch2 = ElemPitchAlgo 
    { initial_stateP  :: st
    , element_trafoP  :: forall drn anno. 
                         Element pch1 drn anno -> Mon st (Element pch2 drn anno)
    }


transformP :: forall st p1 p2 drn anno. 
              ElemPitchAlgo st p1 p2 
           -> Phrase p1 drn anno 
           -> Phrase p2 drn anno
transformP (ElemPitchAlgo { initial_stateP = st0 
                          , element_trafoP = elemT }) = genTransform elemT st0


-- | This is a seems less generally useful than @transformP@ 
-- so we don\'t expose an /algo/.
-- 
collectP :: forall st pch drn anno ac.
            (ac -> pch -> Mon st ac) 
         -> ac 
         -> st
         -> Phrase pch drn anno 
         -> ac
collectP mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note p _ _ _)      = mf ac p
    elementC ac (Rest {})           = pure $ ac
    elementC ac (Spacer {})         = pure $ ac
    elementC ac (Skip {})           = pure $ ac
    elementC ac (Punctuation {})    = pure $ ac



--------------------------------------------------------------------------------
-- Transformation

mapPitch :: (pch1 -> pch2) -> Phrase pch1 drn anno -> Phrase pch2 drn anno
mapPitch fn = ctxMapPitch (\_ p -> fn p)


ctxMapPitch :: (Key -> pch1 -> pch2) 
            -> Phrase pch1 drn anno 
            -> Phrase pch2 drn anno
ctxMapPitch fn = transformP algo 
  where
    algo  = ElemPitchAlgo { initial_stateP    = ()
                          , element_trafoP    = stepE }

    stepE (Note p d a t)    = (\ks -> Note (fn ks p) d a t) <$> asks section_key
    stepE (Rest d)          = pure $ Rest d
    stepE (Spacer d)        = pure $ Spacer d
    stepE (Skip d)          = pure $ Skip d
    stepE (Punctuation s)   = pure $ Punctuation s


foldPitch :: (ac -> pch -> ac) -> ac -> Phrase pch drn anno -> ac
foldPitch fn a0 ph = collectP step a0 () ph
  where
    step ac p   = pure $ fn ac p

--------------------------------------------------------------------------------
-- Duration

data ElemDurationAlgo st drn1 drn2 = ElemDurationAlgo 
    { initial_stateD :: st
    , element_trafoD :: forall pch anno. 
                        Element pch drn1 anno -> Mon st (Element pch drn2 anno)
    }


transformD :: forall st pch d1 d2 anno.
              ElemDurationAlgo st d1 d2 
           -> Phrase pch d1 anno 
           -> Phrase pch d2 anno
transformD (ElemDurationAlgo { initial_stateD = st0 
                             , element_trafoD = elemT }) = genTransform elemT st0


-- | This is a seems less generally useful than @transformD@ 
-- so we don\'t expose an /algo/.
-- 
collectD :: forall st pch drn anno ac.
            (ac -> drn -> Mon st ac) 
         -> ac 
         -> st
         -> Phrase pch drn anno 
         -> ac
collectD mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note _ d _ _)      = mf ac d
    elementC ac (Rest {})           = pure $ ac
    elementC ac (Spacer {})         = pure $ ac
    elementC ac (Skip {})           = pure $ ac
    elementC ac (Punctuation {})    = pure $ ac

--------------------------------------------------------------------------------
-- Transformation

-- Note - increasing or decreasing duration would imply 
-- recalculating bar lines.

mapDuration :: (drn1 -> drn2) -> Phrase pch drn1 anno -> Phrase pch drn2 anno
mapDuration fn = transformD algo 
  where
    algo  = ElemDurationAlgo { initial_stateD   = ()
                             , element_trafoD   = stepE }

    stepE (Note p d a t)        = pure $ Note p (fn d) a t
    stepE (Rest d)              = pure $ Rest (fn d)
    stepE (Spacer d)            = pure $ Spacer (fn d)
    stepE (Skip d)              = pure $ Skip (fn d)
    stepE (Punctuation s)       = pure $ Punctuation s


foldDuration :: (ac -> drn -> ac) -> ac -> Phrase pch drn anno -> ac
foldDuration fn a0 ph = collectD step a0 () ph
  where
    step ac d   = pure $ fn ac d

--------------------------------------------------------------------------------
-- Annotation


data ElemAnnoAlgo st anno1 anno2 = ElemAnnoAlgo 
    { initial_stateA  :: st
    , element_trafoA  :: forall pch drn. 
                         Element pch drn anno1 -> Mon st (Element pch drn anno2)
    }


transformA :: forall st pch drn a1 a2.
              ElemAnnoAlgo st a1 a2
           -> Phrase pch drn a1 
           -> Phrase pch drn a2
transformA (ElemAnnoAlgo { initial_stateA = st0 
                         , element_trafoA = elemT }) = genTransform elemT st0


collectA :: forall st pch drn anno ac.
            (ac -> anno -> Mon st ac) 
         -> ac 
         -> st
         -> Phrase pch drn anno 
         -> ac
collectA mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note _ _ a _)      = mf ac a
    elementC ac (Rest {})           = pure $ ac
    elementC ac (Spacer {})         = pure $ ac
    elementC ac (Skip {})           = pure $ ac
    elementC ac (Punctuation {})    = pure $ ac


--------------------------------------------------------------------------------
-- Transformation


mapAnno :: (anno1 -> anno2) -> Phrase pch drn anno1 -> Phrase pch drn anno2
mapAnno fn = transformA algo 
  where
    algo  = ElemAnnoAlgo { initial_stateA   = ()
                         , element_trafoA   = stepE }

    stepE (Note p d a t)        = pure $ Note p d (fn a) t
    stepE (Rest d)              = pure $ Rest d
    stepE (Spacer d)            = pure $ Spacer d
    stepE (Skip d)              = pure $ Skip d
    stepE (Punctuation s)       = pure $ Punctuation s


foldAnno :: (ac -> anno -> ac) -> ac -> Phrase pch drn anno -> ac
foldAnno fn a0 ph = collectA step a0 () ph
  where
    step ac a   = pure $ fn ac a

--------------------------------------------------------------------------------
-- Pitch and Annotation

data ElemPitchAnnoAlgo st pch1 anno1 pch2 anno2 = ElemPitchAnnoAlgo 
    { initial_statePA :: st
    , element_trafoPA :: 
             forall drn. 
             Element pch1 drn anno1 -> Mon st (Element pch2 drn anno2)
    }


transformPA :: forall st p1 p2 drn a1 a2.
               ElemPitchAnnoAlgo st p1 a1 p2 a2
            -> Phrase p1 drn a1 
            -> Phrase p2 drn a2
transformPA (ElemPitchAnnoAlgo { initial_statePA = st0 
                               , element_trafoPA = elemT }) = 
    genTransform elemT st0


collectPA :: forall st pch drn anno ac.
             (ac -> pch -> anno -> Mon st ac) 
          -> ac 
          -> st
          -> Phrase pch drn anno 
          -> ac
collectPA mf = genCollect elementC
  where
    elementC :: ac -> Element pch drn anno -> Mon st ac
    elementC ac (Note p _ a _)      = mf ac p a
    elementC ac (Rest {})           = pure $ ac
    elementC ac (Spacer {})         = pure $ ac
    elementC ac (Skip {})           = pure $ ac
    elementC ac (Punctuation {})    = pure $ ac



--------------------------------------------------------------------------------
-- Pitch Anno Transformation


mapPitchAnno :: (p1 -> a1 -> (p2,a2)) -> Phrase p1 drn a1 -> Phrase p2 drn a2
mapPitchAnno fn = transformPA algo 
  where
    algo  = ElemPitchAnnoAlgo { initial_statePA   = ()
                              , element_trafoPA   = stepE }

    stepE (Note p d a t)    = let (p1,a1) = fn p a in pure $ Note p1 d a1 t
    stepE (Rest d)          = pure $ Rest d
    stepE (Spacer d)        = pure $ Spacer d
    stepE (Skip d)          = pure $ Skip d
    stepE (Punctuation s)   = pure $ Punctuation s

foldPitchAnno :: (ac -> pch -> anno -> ac) -> ac -> Phrase pch drn anno -> ac
foldPitchAnno fn a0 ph = collectPA step a0 () ph
  where
    step ac p a   = pure $ fn ac p a


--------------------------------------------------------------------------------
-- Punctuation

censorPunctuation :: Phrase pch drn anno -> Phrase pch drn anno
censorPunctuation (Phrase info bs) = Phrase info (map bar1 bs)
  where
    bar1 (Bar cs)               = Bar $ catMaybes $ map noteGroup1 cs

    noteGroup1 (Atom e)         = censor e >>= (return . Atom)
    noteGroup1 (Tuplet spec es) = let xs = catMaybes $ map censor es
                                  in if null xs then Nothing 
                                                else Just $ Tuplet spec xs


    censor (Punctuation {})     = Nothing
    censor e                    = Just e


--------------------------------------------------------------------------------
-- Markup

censorAnno :: Phrase pch drn anno -> Phrase pch drn ()
censorAnno (Phrase info bs) = Phrase info (map bar1 bs)
  where
    bar1 (Bar cs)               = Bar $ map noteGroup1 cs

    noteGroup1 (Atom e)         = Atom $ changeNote e
    noteGroup1 (Tuplet spec es) = Tuplet spec $ map changeNote es

    changeNote (Note p d _ t)   = Note p d () t
    changeNote (Rest d)         = Rest d
    changeNote (Spacer d)       = Spacer d
    changeNote (Skip d)         = Skip d
    changeNote (Punctuation s)  = Punctuation s


--------------------------------------------------------------------------------
-- Skip to rest

changeSkipToRest :: Phrase pch drn anno -> Phrase pch drn anno
changeSkipToRest (Phrase info bs) = Phrase info (map bar1 bs)
  where
    bar1 (Bar cs)               = Bar $ map noteGroup1 cs

    noteGroup1 (Atom e)         = Atom $ changeSkip e
    noteGroup1 (Tuplet spec es) = Tuplet spec $ map changeSkip es

    changeSkip (Skip d)         = Rest d
    changeSkip e                = e
