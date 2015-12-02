{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE RankNTypes                 #-}
{-# OPTIONS -Wall #-}

--------------------------------------------------------------------------------
-- |
-- Module      :  Payasan.Base.Monophonic.Internal.Traversals
-- Copyright   :  (c) Stephen Tetley 2015
-- License     :  BSD3
--
-- Maintainer  :  stephen.tetley@gmail.com
-- Stability   :  unstable
-- Portability :  GHC
--
-- Generic traversals of Mono syntax.
--
--------------------------------------------------------------------------------

module Payasan.Base.Monophonic.Internal.Traversals
  (
    Mon 

  , nth
  , take
  , drop

  , MonoPitchAlgo(..)
  , transformP
  , collectP
  , mapPitch
  , ctxMapPitch   -- TEMP ?
  , foldPitch 


  , MonoDurationAlgo(..)
  , transformD
  , collectD
  , mapDuration
  , foldDuration

  , MonoAnnoAlgo(..)
  , transformA
  , collectA
  , mapAnno
  , foldAnno

  , MonoPitchAnnoAlgo(..)
  , transformPA
  , collectPA
  , mapPitchAnno
  , foldPitchAnno

  , censorPunctuation
  , censorAnno
  , skipToRest

  ) where



import Payasan.Base.Monophonic.Internal.RecalcBars
import Payasan.Base.Monophonic.Internal.Syntax

import Payasan.Base.Internal.CommonSyntax
import Payasan.Base.Internal.RewriteMonad

import Data.Foldable (foldlM)
import Data.Maybe

import Prelude hiding (take, drop)

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

-- Length changing traversals will be easier going through 
--



-- | nth might be counter-intuitive in the presence of 
-- triplets...
--
nth :: forall pch drn anno.
       Int -> Phrase pch drn anno -> Maybe (Element pch drn anno)
nth _ (Phrase _ [])             = Nothing
nth i (Phrase _ (Bar b1:bs))    = step1 0 b1 bs 
  where
    step1 :: Int -> [NoteGroup pch drn anno] -> [Bar pch drn anno] 
          -> Maybe (Element pch drn anno)
    step1 _ []      []          = Nothing
    step1 n []      (Bar r1:rs) = step1 n r1 rs
    step1 n (g1:gs) rs          = case step2 n g1 of
        Left n1 -> step1 n1 gs rs
        Right a -> Just a

    step2 n (Atom e)  
        | n == i                = Right e
        | otherwise             = Left $ n+1
                  
    step2 n (Tuplet _ es)       = step3 n es

    step3 n (e:es)              
        | n == i                = Right e
        | otherwise             = step3 (n+1) es

    step3 n []                  = Left n


-- nth suggests take and drop


-- | Tuplet splitting is not properly implemented yet as it 
-- should modify the spec
--
-- NOTE - the covoluted nature of the implementations of take 
-- and drop suggest nested tuplets may be more trouble than they
-- are worth...
--
take :: forall pch anno.
        Int -> StdMonoPhrase2 pch anno -> StdMonoPhrase2 pch anno
take i = viaNoteList fn 
  where
    fn (NoteList info xs)       = NoteList info $ step1 0 xs

    step1 :: Int -> [StdMonoNoteGroup2 pch anno]-> [StdMonoNoteGroup2 pch anno]
    step1 n _                   | n >= i = []
    step1 _ []                  = []
    step1 n (Atom e:gs)         = (Atom e) : step1 (n+1) gs
    step1 n (Tuplet spec es:gs) = 
        let (n1,es1) = step2 n es
        in if n1 >= i then [Tuplet spec es1]
                      else (Tuplet spec es1) : step1 n1 gs

    step2 n []                  = (n,[])
    step2 n _                   | n >= i = (n,[])
    step2 n (e:es)              = let (n1,ys) = step2 (n+1) es in (n1,e:ys)


drop :: forall pch anno.
        Int -> StdMonoPhrase2 pch anno -> StdMonoPhrase2 pch anno
drop i = viaNoteList fn 
  where
    fn (NoteList info xs)       = NoteList info $ step1 i xs

    step1 :: Int -> [StdMonoNoteGroup2 pch anno]-> [StdMonoNoteGroup2 pch anno]
    step1 n xs                  | n <= 0 = xs
    step1 _ []                  = []
    step1 n (Atom {}:gs)        = step1 (n-1) gs
    step1 _ (Tuplet {}:_)       = error "Tuplet"
    -- TODO - it will be better to define a set of operations that 
    -- work on tuplets rather than do ad hoc destructuring here
  


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


data MonoPitchAlgo st pch1 pch2 = MonoPitchAlgo 
    { initial_stateP  :: st
    , element_trafoP  :: forall drn anno. 
                         Element pch1 drn anno -> Mon st (Element pch2 drn anno)
    }


transformP :: forall st p1 p2 drn anno. 
              MonoPitchAlgo st p1 p2 
           -> Phrase p1 drn anno 
           -> Phrase p2 drn anno
transformP (MonoPitchAlgo { initial_stateP = st0 
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
    algo  = MonoPitchAlgo { initial_stateP    = ()
                          , element_trafoP    = stepE 
                          }

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

data MonoDurationAlgo st drn1 drn2 = MonoDurationAlgo 
    { initial_stateD :: st
    , element_trafoD :: forall pch anno. 
                        Element pch drn1 anno -> Mon st (Element pch drn2 anno)
    }


transformD :: forall st pch d1 d2 anno.
              MonoDurationAlgo st d1 d2 
           -> Phrase pch d1 anno 
           -> Phrase pch d2 anno
transformD (MonoDurationAlgo { initial_stateD = st0 
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
    algo  = MonoDurationAlgo { initial_stateD   = ()
                             , element_trafoD   = stepE 
                             }

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


data MonoAnnoAlgo st anno1 anno2 = MonoAnnoAlgo 
    { initial_stateA  :: st
    , element_trafoA  :: forall pch drn. 
                         Element pch drn anno1 -> Mon st (Element pch drn anno2)
    }


transformA :: forall st pch drn a1 a2.
              MonoAnnoAlgo st a1 a2
           -> Phrase pch drn a1 
           -> Phrase pch drn a2
transformA (MonoAnnoAlgo { initial_stateA = st0 
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
    algo  = MonoAnnoAlgo { initial_stateA   = ()
                         , element_trafoA   = stepE 
                         }

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

data MonoPitchAnnoAlgo st pch1 anno1 pch2 anno2 = MonoPitchAnnoAlgo 
    { initial_statePA :: st
    , element_trafoPA :: 
             forall drn. 
             Element pch1 drn anno1 -> Mon st (Element pch2 drn anno2)
    }


transformPA :: forall st p1 p2 drn a1 a2.
               MonoPitchAnnoAlgo st p1 a1 p2 a2
            -> Phrase p1 drn a1 
            -> Phrase p2 drn a2
transformPA (MonoPitchAnnoAlgo { initial_statePA = st0 
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
    algo  = MonoPitchAnnoAlgo { initial_statePA   = ()
                              , element_trafoPA   = stepE 
                              }

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

skipToRest :: Phrase pch drn anno -> Phrase pch drn anno
skipToRest (Phrase info bs) = Phrase info (map bar1 bs)
  where
    bar1 (Bar cs)               = Bar $ map noteGroup1 cs

    noteGroup1 (Atom e)         = Atom $ changeSkip e
    noteGroup1 (Tuplet spec es) = Tuplet spec $ map changeSkip es

    changeSkip (Skip d)         = Rest d
    changeSkip e                = e
