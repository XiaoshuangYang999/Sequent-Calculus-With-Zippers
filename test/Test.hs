
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import General
import CPL
import IPL
import K
import K4
import S4
import PForm
import MForm


main :: IO ()
main = hspec $ do
  describe "Unit tests" $ do
    describe "G3C.isProvableZ" $ do
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
    describe "not.G3C.isProvableZ" $ do
      it "Bot" $ not . isProvableZ classical $ BotP
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableZ classical $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableZ classical $ ConP r (DisP p (negP p))

    describe "G3C.isProvableT" $ do
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
    describe "not.G3C.isProvableT" $ do
      it "Bot" $ not . isProvableT classical $ BotP
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableT classical $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableT classical $ ConP r (DisP p (negP p))

    describe "G3I.isProvableZ" $ do
      it "Top" $ isProvableZ intui topP
      it (show t1) $ isProvableZ intui t1
      it (show t2) $ isProvableZ intui t2
      it (show dnR) $ isProvableZ intui dnR
      it (show t3) $ isProvableZ intui t3
      it (show phi) $ isProvableZ intui phi
      it (show t4) $ isProvableZ intui t4
    describe "not.G3I.isProvableZ" $ do
      it "Bot" $ not . isProvableZ intui $ BotP
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableZ intui $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableZ intui $ ConP r (DisP p (negP p))
      it ("Double negation: " ++ show dn) $ not $ isProvableZ intui dn
      it ("Excluded middle: " ++ show em) $ not $ isProvableZ intui em
      it ("Pierce's law: " ++ show pierce) $ not $ isProvableZ intui pierce

    describe "G3I.isProvableT" $ do
      it "Top" $ isProvableT intui topP
      it (show t1) $ isProvableT intui t1
      it (show t2) $ isProvableT intui t2
      it (show dnR) $ isProvableT intui dnR
      it (show t3) $ isProvableT intui t3
      it (show phi) $ isProvableT intui phi
      it (show t4) $ isProvableT intui t4
    describe "not.G3I.isProvableT" $ do
      it "Bot" $ not . isProvableT intui $ BotP
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableT intui $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableT intui $ ConP r (DisP p (negP p))
      it ("Double negation: " ++ show dn) $ not $ isProvableT intui dn
      it ("Excluded middle: " ++ show em) $ not $ isProvableT intui em
      it ("Pierce's law: " ++ show pierce) $ not $ isProvableT intui pierce

    describe "G3K.isProvableZ" $ do
      it "Top" $ isProvableZ k $ pTom topP
      it (show t1) $ isProvableZ k $ pTom t1
      it ("Double negation: " ++ show dn) $ isProvableZ k $ pTom dn
      it ("Excluded middle: " ++ show em) $ isProvableZ k $ pTom em
      it ("Pierce's law: " ++ show pierce) $ isProvableZ k $ pTom pierce
      it (show t2) $ isProvableZ k $ pTom t2
      it (show dnR) $ isProvableZ k $ pTom dnR
      it (show t3) $ isProvableZ k $ pTom t3
      it (show phi) $ isProvableZ k $ pTom phi
      it (show t4) $ isProvableZ k $ pTom t4
      it "Box top" $ isProvableZ k $ Box topM
      it "K axiom" $ isProvableZ k kaxiom
    describe "not.G3K.isProvableZ" $ do
      it "Bot" $ not $ isProvableZ k BotM
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableZ k $ pTom $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableZ k $ pTom $ ConP r (DisP p (negP p))
      it ("Box " ++ show BotM) $ not $ isProvableZ k $ Box BotM
      it (show ktest) $ not $ isProvableZ k ktest

    describe "G3K.isProvableT" $ do
      it "Top" $ isProvableT k $ pTom topP
      it (show t1) $ isProvableT k $ pTom t1
      it ("Double negation: " ++ show dn) $ isProvableT k $ pTom dn
      it ("Excluded middle: " ++ show em) $ isProvableT k $ pTom em
      it ("Pierce's law: " ++ show pierce) $ isProvableT k $ pTom pierce
      it (show t2) $ isProvableT k $ pTom t2
      it (show dnR) $ isProvableT k $ pTom dnR
      it (show t3) $ isProvableT k $ pTom t3
      it (show phi) $ isProvableT k $ pTom phi
      it (show t4) $ isProvableT k $ pTom t4
      it "Box top" $ isProvableT k $ Box topM
      it "K axiom" $ isProvableT k kaxiom
    describe "not.G3K.isProvableT" $ do
      it "Bot" $ not $ isProvableT k BotM
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableT k $ pTom $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableT k $ pTom $ ConP r (DisP p (negP p))
      it ("Box " ++ show BotM) $ not $ isProvableT k $ Box BotM
      it (show ktest) $ not $ isProvableT k ktest

-- K4 tests
    describe "G3K4.isProvableZ" $ do
      it "K axiom" $ isProvableZ kfour kaxiom
      it "4 axiom" $ isProvableZ kfour fouraxiom
      it "boxToMoreBox 4" $ isProvableZ kfour (boxToMoreBox 4)
    describe "G3K4.isProvableT" $ do
      it "K axiom" $ isProvableT kfour kaxiom
      it "4 axiom" $ isProvableT kfour fouraxiom
      it "boxToMoreBox 4" $ isProvableT kfour (boxToMoreBox 4)
    describe "not.G3K4.isProvableZ" $ do
      it "T axiom" $ not $ isProvableZ kfour taxiom
      it "Lob axiom" $ not $ isProvableZ kfour lobaxiom
      it "Consistency axiom" $ not $ isProvableZ kfour consistency
      it "boxToFewerBox 4" $ not $ isProvableZ kfour (boxToFewerBox 4)
    describe "not.G3K4.isProvableT" $ do
      it "T axiom" $ not $ isProvableT kfour taxiom
      it "Lob axiom" $ not $ isProvableT kfour lobaxiom
      it "Consistency axiom" $ not $ isProvableT kfour consistency
      it "boxToFewerBox 4" $ not $ isProvableT kfour (boxToFewerBox 4)

-- S4 tests
    describe "G3S4.isProvableZ" $ do
      it "K axiom" $ isProvableZ sfour kaxiom
      it "4 axiom" $ isProvableZ sfour fouraxiom
      it "T axiom" $ isProvableZ sfour taxiom
      it "Consistency axiom" $ isProvableZ sfour consistency
      it "boxToMoreBox 4" $ isProvableZ sfour (boxToMoreBox 4)
      it "boxToFewerBox 4" $ isProvableZ sfour (boxToFewerBox 4)
    describe "G3S4.isProvableT" $ do
      it "K axiom" $ isProvableT sfour kaxiom
      it "4 axiom" $ isProvableT sfour fouraxiom
      it "T axiom" $ isProvableT sfour taxiom
      it "Consistency axiom" $ isProvableT sfour consistency
      it "boxToMoreBox 4" $ isProvableT sfour (boxToMoreBox 4)
      it "boxToFewerBox 4" $ isProvableT sfour (boxToFewerBox 4)
    describe "not.G3S4.isProvableZ" $ do
      it "Lob axiom" $ not $ isProvableZ sfour lobaxiom
    describe "not.G3S4.isProvableT" $ do
      it "Lob axiom" $ not $ isProvableT sfour lobaxiom

    describe "What is provable in K is also provable in K4" $ do
      prop "GenZ" $
        \ f -> isProvableZ k f  <= isProvableZ kfour f
      prop "GenT" $
        \ f -> isProvableT k f  <= isProvableT kfour f

    describe "What is provable in K4 is also provable in S4" $ do
      prop "GenZ" $
        \ f -> isProvableZ kfour f  <= isProvableZ sfour f
      prop "GenT" $
        \ f -> isProvableT kfour f  <= isProvableT sfour f


  describe "Additional Correctness Tests" $ do
    describe "Equivalence between two provers" $ do
      modifyMaxSuccess (const 1000) $
        prop "In G3C" $
          \ f -> isProvableZ classical f  == isProvableT classical f
      modifyMaxSuccess (const 1000) $
        prop "In G3I" $
          \ f -> isProvableZ intui f  == isProvableT intui f
      modifyMaxSuccess (const 1000) $
        prop "In G3K" $
          \ f -> isProvableZ k f  == isProvableT k f
      modifyMaxSuccess (const 1000) $
        prop "In G3K4" $
          \ f -> isProvableZ kfour f  == isProvableT kfour f
      modifyMaxSuccess (const 1000) $
        prop "In G3S4" $
          \ f -> isProvableZ sfour f  == isProvableT sfour f

    describe "Propositional tautologies hold in modal logics" $ do
      modifyMaxSuccess (const 1000) $
        prop "In G3K" $
          \ f -> isProvableZ classical f  == isProvableZ k (pTom f)
      modifyMaxSuccess (const 1000) $
        prop "In G3K4" $
          \ f -> isProvableZ classical f  == isProvableZ kfour (pTom f)
      modifyMaxSuccess (const 1000) $
        prop "In G3S4" $
          \ f -> isProvableZ classical f  == isProvableZ sfour (pTom f)

    describe "Proofs are at most binary" $ do
        prop "zipper for G3C" $
          \ f -> all hasLeqTwoChildren $ proveZ classical f
        prop "tree for G3C" $
          \ f -> all hasLeqTwoChildren $ proveT classical f
        prop "zipper for G3I" $
          \ f -> all hasLeqTwoChildren $ proveZ intui f
        prop "tree for G3I" $
          \ f -> all hasLeqTwoChildren $ proveT intui f
        prop "zipper for G3K" $
          \ f -> all hasLeqTwoChildren $ proveZ k f
        prop "tree for G3K" $
          \ f -> all hasLeqTwoChildren $ proveT k f

    describe "If f and g isProvable, then Con f g isProvable" $ do
        prop "zipper for G3C" $
          \ f g -> (isProvableZ classical f && isProvableZ classical g) <= isProvableZ classical (ConP f g)
        prop "tree for G3C" $
          \ f g -> (isProvableT classical f && isProvableT classical g) <= isProvableT classical (ConP f g)
        prop "zipper for G3I" $
          \ f g -> (isProvableZ intui f && isProvableZ intui g) <= isProvableZ intui (ConP f g)
        prop "tree for G3I" $
          \ f g -> (isProvableT intui f && isProvableT intui g) <= isProvableT intui (ConP f g)
        prop "zipper for G3K" $
          \ f g -> (isProvableZ k f && isProvableZ k g) <= isProvableZ k (ConM f g)
        prop "tree for G3K" $
          \ f g -> (isProvableT k f && isProvableT k g) <= isProvableT k (ConM f g)

    describe "If f isProvable in G3C, then neg neg f isProvable in G3I" $ do
        prop "zipper" $
          \ f -> isProvableZ classical f <= isProvableZ intui (negP (negP f))
        prop "tree" $
          \ f -> isProvableT classical f <= isProvableT intui (negP (negP f))

-- The test is taking too much time