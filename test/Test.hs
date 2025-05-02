module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import General
import CPL
import IPL
import K
import K4
import GL
import S4
import PForm
import MForm

main :: IO ()
main = hspec $ do
  describe "Unit tests" $ do
    describe "CPL.isProvableZ" $ do
      it "Top" $ isProvableZ classical topP
      it (show t1) $ isProvableZ classical t1
      it ("Double negation: " ++ show dn) $ isProvableZ classical dn
      it ("Excluded middle: " ++ show em) $ isProvableZ classical em
      it ("Pierce's law: " ++ show pierce) $ isProvableZ classical pierce
      it (show t2) $ isProvableZ classical t2
      it (show dnR) $ isProvableZ classical dnR
      it (show t3) $ isProvableZ classical t3
      it (show phi) $ isProvableZ classical phi
      it (show t4) $ isProvableZ classical t4
    describe "not.CPL.isProvableZ" $ do
      it "Bot" $ not . isProvableZ classical $ BotP
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableZ classical $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableZ classical $ ConP r (DisP p (negP p))

    describe "CPL.isProvableT" $ do
      it "Top" $ isProvableT classical topP
      it (show t1) $ isProvableT classical t1
      it ("Double negation: " ++ show dn) $ isProvableT classical dn
      it ("Excluded middle: " ++ show em) $ isProvableT classical em
      it ("Pierce's law: " ++ show pierce) $ isProvableT classical pierce
      it (show t2) $ isProvableT classical t2
      it (show dnR) $ isProvableT classical dnR
      it (show t3) $ isProvableT classical t3
      it (show phi) $ isProvableT classical phi
      it (show t4) $ isProvableT classical t4
    describe "not.CPL.isProvableT" $ do
      it "Bot" $ not . isProvableT classical $ BotP
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableT classical $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableT classical $ ConP r (DisP p (negP p))

    describe "IPL.isProvableZ" $ do
      it "Top" $ isProvableZ intui topP
      it (show t1) $ isProvableZ intui t1
      it (show t2) $ isProvableZ intui t2
      it (show dnR) $ isProvableZ intui dnR
      it (show t3) $ isProvableZ intui t3
      it (show phi) $ isProvableZ intui phi
      it (show t4) $ isProvableZ intui t4
    describe "not.IPL.isProvableZ" $ do
      it "Bot" $ not . isProvableZ intui $ BotP
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableZ intui $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableZ intui $ ConP r (DisP p (negP p))
      it ("Double negation: " ++ show dn) $ not $ isProvableZ intui dn
      it ("Excluded middle: " ++ show em) $ not $ isProvableZ intui em
      it ("Pierce's law: " ++ show pierce) $ not $ isProvableZ intui pierce

    describe "IPL.isProvableT" $ do
      it "Top" $ isProvableT intui topP
      it (show t1) $ isProvableT intui t1
      it (show t2) $ isProvableT intui t2
      it (show dnR) $ isProvableT intui dnR
      it (show t3) $ isProvableT intui t3
      it (show phi) $ isProvableT intui phi
      it (show t4) $ isProvableT intui t4
    describe "not.IPL.isProvableT" $ do
      it "Bot" $ not . isProvableT intui $ BotP
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableT intui $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableT intui $ ConP r (DisP p (negP p))
      it ("Double negation: " ++ show dn) $ not $ isProvableT intui dn
      it ("Excluded middle: " ++ show em) $ not $ isProvableT intui em
      it ("Pierce's law: " ++ show pierce) $ not $ isProvableT intui pierce

    describe "K.isProvableZ" $ do
      -- it "Top" $ isProvableZ k $ pTom topP
      -- it (show t1) $ isProvableZ k $ pTom t1
      -- it ("Double negation: " ++ show dn) $ isProvableZ k $ pTom dn
      -- it ("Excluded middle: " ++ show em) $ isProvableZ k $ pTom em
      -- it ("Pierce's law: " ++ show pierce) $ isProvableZ k $ pTom pierce
      -- it (show t2) $ isProvableZ k $ pTom t2
      -- it (show dnR) $ isProvableZ k $ pTom dnR
      -- it (show t3) $ isProvableZ k $ pTom t3
      -- it (show phi) $ isProvableZ k $ pTom phi
      -- it (show t4) $ isProvableZ k $ pTom t4
      it "Box top" $ isProvableZ k $ Box topM
      it "K axiom" $ isProvableZ k kaxiom
    describe "not.K.isProvableZ" $ do
      it "Bot" $ not $ isProvableZ k BotM
      -- it (show (ConP p (negP p)) ++ " ") $ not . isProvableZ k $ pTom $ ConP p (negP p)
      -- it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableZ k $ pTom $ ConP r (DisP p (negP p))
      it ("Box " ++ show BotM) $ not $ isProvableZ k $ Box BotM
      it (show ktest) $ not $ isProvableZ k ktest
      it "T axiom" $ not $ isProvableZ k taxiom
      it "4 axiom" $ not $ isProvableZ k fouraxiom
      it "Lob axiom" $ not $ isProvableZ k lobaxiom

    describe "K.isProvableT" $ do
      -- it "Top" $ isProvableT k $ pTom topP
      -- it (show t1) $ isProvableT k $ pTom t1
      -- it ("Double negation: " ++ show dn) $ isProvableT k $ pTom dn
      -- it ("Excluded middle: " ++ show em) $ isProvableT k $ pTom em
      -- it ("Pierce's law: " ++ show pierce) $ isProvableT k $ pTom pierce
      -- it (show t2) $ isProvableT k $ pTom t2
      -- it (show dnR) $ isProvableT k $ pTom dnR
      -- it (show t3) $ isProvableT k $ pTom t3
      -- it (show phi) $ isProvableT k $ pTom phi
      -- it (show t4) $ isProvableT k $ pTom t4
      it "Box top" $ isProvableT k $ Box topM
      it "K axiom" $ isProvableT k kaxiom
    describe "not.K.isProvableT" $ do
      it "Bot" $ not $ isProvableT k BotM
      -- it (show (ConP p (negP p)) ++ " ") $ not . isProvableT k $ pTom $ ConP p (negP p)
      -- it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableT k $ pTom $ ConP r (DisP p (negP p))
      it ("Box " ++ show BotM) $ not $ isProvableT k $ Box BotM
      it (show ktest) $ not $ isProvableT k ktest
      it "T axiom" $ not $ isProvableT k taxiom
      it "4 axiom" $ not $ isProvableT k fouraxiom
      it "Lob axiom" $ not $ isProvableT k lobaxiom

    -- K4 tests
    describe "K4.isProvableZ" $ do
      it "K axiom" $ isProvableZ kfour kaxiom
      it "4 axiom" $ isProvableZ kfour fouraxiom
      it "boxToMoreBox 4" $ isProvableZ kfour (boxToMoreBox 4)
    describe "not.K4.isProvableZ" $ do
      it "T axiom" $ not $ isProvableZ kfour taxiom
      it "Lob axiom" $ not $ isProvableZ kfour lobaxiom
      it "Consistency axiom" $ not $ isProvableZ kfour consistency
      it "boxToFewerBox 4" $ not $ isProvableZ kfour (boxToFewerBox 4)

    describe "K4.isProvableT" $ do
      it "K axiom" $ isProvableT kfour kaxiom
      it "4 axiom" $ isProvableT kfour fouraxiom
      it "boxToMoreBox 4" $ isProvableT kfour (boxToMoreBox 4)
    describe "not.K4.isProvableT" $ do
      it "T axiom" $ not $ isProvableT kfour taxiom
      it "Lob axiom" $ not $ isProvableT kfour lobaxiom
      it "Consistency axiom" $ not $ isProvableT kfour consistency
      it "boxToFewerBox 4" $ not $ isProvableT kfour (boxToFewerBox 4)

    -- GL tests
    describe "GL.isProvableZ" $ do
      it "K axiom" $ isProvableZ gl kaxiom
      it "4 axiom" $ isProvableZ gl fouraxiom
      it "Lob axiom" $ isProvableZ gl lobaxiom      
      it "boxToMoreBox 4" $ isProvableZ gl (boxToMoreBox 4)
    describe "not.GL.isProvableZ" $ do
      it "T axiom" $ not $ isProvableZ gl taxiom
      it "Consistency axiom" $ not $ isProvableZ gl consistency
      it "boxToFewerBox 4" $ not $ isProvableZ gl (boxToFewerBox 4)

    describe "GL.isProvableT" $ do
      it "K axiom" $ isProvableT gl kaxiom
      it "4 axiom" $ isProvableT gl fouraxiom
      it "Lob axiom" $ isProvableT gl lobaxiom      
      it "boxToMoreBox 4" $ isProvableT gl (boxToMoreBox 4)
      
    describe "not.GL.isProvableT" $ do
      it "T axiom" $ not $ isProvableT gl taxiom
      it "Consistency axiom" $ not $ isProvableT gl consistency
      it "boxToFewerBox 4" $ not $ isProvableT gl (boxToFewerBox 4)

    -- S4 tests
    describe "S4.isProvableZ" $ do
      it "K axiom" $ isProvableZ sfour kaxiom
      it "4 axiom" $ isProvableZ sfour fouraxiom
      it "T axiom" $ isProvableZ sfour taxiom
      it "Consistency axiom" $ isProvableZ sfour consistency
      it "boxToMoreBox 4" $ isProvableZ sfour (boxToMoreBox 4)
      it "boxToFewerBox 4" $ isProvableZ sfour (boxToFewerBox 4)
    describe "not.S4.isProvableZ" $ do
      it "Lob axiom" $ not $ isProvableZ sfour lobaxiom

    describe "S4.isProvableT" $ do
      it "K axiom" $ isProvableT sfour kaxiom
      it "4 axiom" $ isProvableT sfour fouraxiom
      it "T axiom" $ isProvableT sfour taxiom
      it "Consistency axiom" $ isProvableT sfour consistency
      it "boxToMoreBox 4" $ isProvableT sfour (boxToMoreBox 4)
      it "boxToFewerBox 4" $ isProvableT sfour (boxToFewerBox 4)
    describe "not.S4.isProvableT" $ do
      it "Lob axiom" $ not $ isProvableT sfour lobaxiom

  -- Set a time limit.
  -- Test cases will be discarded if they take more than 5 seconds.
  let limit = 5 * 1000000 -- in microseconds

  describe "Integration tests" $ do
    describe "Equivalence between GenZ and GenT" $ modifyMaxSuccess (const 1000) $ do
      prop "In CPL" $
        \ f -> discardAfter limit $ isProvableZ classical f === isProvableT classical f
      prop "In IPL" $
        \ f -> discardAfter limit $ isProvableZ intui f === isProvableT intui f
      prop "In K" $
        \ f -> discardAfter limit $ isProvableZ k f === isProvableT k f
      prop "In K4" $
        \ f -> discardAfter limit $ isProvableZ kfour f === isProvableT kfour f
      prop "In S4" $
        \ f -> discardAfter limit $ isProvableZ sfour f === isProvableT sfour f

    describe "Proofs are at most binary" $ do
        let hasLeqTwoChildren (Node _ Nothing) = True
            hasLeqTwoChildren (Node _ (Just (_, ts))) = length ts <= 2 && all hasLeqTwoChildren ts
        prop "GenZ for CPL" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ classical f
        prop "GenT for CPL" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT classical f
        prop "GenZ for IPL" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ intui f
        prop "GenT for IPL" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT intui f
        prop "GenZ for K" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ k f
        prop "GenT for K" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT k f
        prop "GenZ for K4" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ kfour f
        prop "GenT for K4" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT kfour f
        prop "GenZ for S4" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveZ sfour f
        prop "GenT for S4" $
          \ f -> discardAfter limit $ all hasLeqTwoChildren $ proveT sfour f

    -- to ensure laziness
    let implies x = if x then id else const True

    describe "If f and g isProvable, then Con f g isProvable" $ do
        prop "GenZ for CPL" $
          \ f g -> discardAfter limit $ (isProvableZ classical f && isProvableZ classical g) `implies` isProvableZ classical (ConP f g)
        prop "GenT for CPL" $
          \ f g -> discardAfter limit $ (isProvableT classical f && isProvableT classical g) `implies` isProvableT classical (ConP f g)
        prop "GenZ for IPL" $
          \ f g -> discardAfter limit $ (isProvableZ intui f && isProvableZ intui g) `implies` isProvableZ intui (ConP f g)
        prop "GenT for IPL" $
          \ f g -> discardAfter limit $ (isProvableT intui f && isProvableT intui g) `implies` isProvableT intui (ConP f g)
        prop "GenZ for K" $
          \ f g -> discardAfter limit $ (isProvableZ k f && isProvableZ k g) `implies` isProvableZ k (ConM f g)
        prop "GenT for K" $
          \ f g -> discardAfter limit $ (isProvableT k f && isProvableT k g) `implies` isProvableT k (ConM f g)
        prop "GenZ for K4" $
          \ f g -> discardAfter limit $ (isProvableZ kfour f && isProvableZ kfour g) `implies` isProvableZ kfour (ConM f g)
        prop "GenT for K4" $
          \ f g -> discardAfter limit $ (isProvableT kfour f && isProvableT kfour g) `implies` isProvableT kfour (ConM f g)
        prop "GenZ for S4" $
          \ f g -> discardAfter limit $ (isProvableZ sfour f && isProvableZ sfour g) `implies` isProvableZ sfour (ConM f g)
        prop "GenT for S4" $
          \ f g -> discardAfter limit $ (isProvableT sfour f && isProvableT sfour g) `implies` isProvableT sfour (ConM f g)

    describe "If f isProvable in CPL, then neg neg f isProvable in IPL" $ do
        prop "GenZ" $
          \ f -> discardAfter limit $ isProvableZ classical f `implies` isProvableZ intui (negP (negP f))
        prop "GenT" $
          \ f -> discardAfter limit $ isProvableT classical f `implies` isProvableT intui (negP (negP f))

    describe "Propositional tautologies in modal logics (GenZ)" $ modifyMaxSuccess (const 1000) $ do
      prop "K" $
        \ f -> discardAfter limit $ isProvableZ classical f === isProvableZ k (pTom f)
      prop "K4" $
        \ f -> discardAfter limit $ isProvableZ classical f === isProvableZ kfour (pTom f)
      prop "S4" $
        \ f -> discardAfter limit $ isProvableZ classical f === isProvableZ sfour (pTom f)

    describe "What is provable in K is also provable in K4" $ do
      prop "GenZ" $
        \ f -> discardAfter limit $ isProvableZ k f `implies` isProvableZ kfour f
      prop "GenT" $
        \ f -> discardAfter limit $ isProvableT k f `implies` isProvableT kfour f

    describe "What is provable in K4 is also provable in S4" $ do
      prop "GenZ" $
        \ f -> discardAfter limit $ isProvableZ kfour f `implies` isProvableZ sfour f
      prop "GenT" $
        \ f -> discardAfter limit $ isProvableT kfour f `implies` isProvableT sfour f
