
module Main where

import Test.Hspec
import Test.Hspec.QuickCheck
import General
import CPL
import IPL
import ML
import Example


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
      it "Top" $ isProvableZ modal $ pTom topP
      it (show t1) $ isProvableZ modal $ pTom t1
      it ("Double negation: " ++ show dn) $ isProvableZ modal $ pTom dn
      it ("Excluded middle: " ++ show em) $ isProvableZ modal $ pTom em
      it ("Pierce's law: " ++ show pierce) $ isProvableZ modal $ pTom pierce
      it (show t2) $ isProvableZ modal $ pTom t2
      it (show dnR) $ isProvableZ modal $ pTom dnR
      it (show t3) $ isProvableZ modal $ pTom t3
      it (show phi) $ isProvableZ modal $ pTom phi
      it (show t4) $ isProvableZ modal $ pTom t4
      it "Box top" $ isProvableZ modal $ Box topM
      it "K axiom" $ isProvableZ modal kaxiom
    describe "not.G3K.isProvableZ" $ do
      it "Bot" $ not $ isProvableZ modal BotM
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableZ modal $ pTom $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableZ modal $ pTom $ ConP r (DisP p (negP p))
      it ("Box " ++ show BotM) $ not $ isProvableZ modal $ Box BotM
      it (show ktest) $ not $ isProvableZ modal ktest

    describe "G3K.isProvableT" $ do
      it "Top" $ isProvableT modal $ pTom topP
      it (show t1) $ isProvableT modal $ pTom t1
      it ("Double negation: " ++ show dn) $ isProvableT modal $ pTom dn
      it ("Excluded middle: " ++ show em) $ isProvableT modal $ pTom em
      it ("Pierce's law: " ++ show pierce) $ isProvableT modal $ pTom pierce
      it (show t2) $ isProvableT modal $ pTom t2
      it (show dnR) $ isProvableT modal $ pTom dnR
      it (show t3) $ isProvableT modal $ pTom t3
      it (show phi) $ isProvableT modal $ pTom phi
      it (show t4) $ isProvableT modal $ pTom t4
      it "Box top" $ isProvableT modal $ Box topM
      it "K axiom" $ isProvableT modal kaxiom
    describe "not.G3K.isProvableT" $ do
      it "Bot" $ not $ isProvableT modal BotM
      it (show (ConP p (negP p)) ++ " ") $ not . isProvableT modal $ pTom $ ConP p (negP p)
      it (show (ConP r (DisP p (negP p))) ++ " ") $ not . isProvableT modal $ pTom $ ConP r (DisP p (negP p))
      it ("Box " ++ show BotM) $ not $ isProvableT modal $ Box BotM
      it (show ktest) $ not $ isProvableT modal ktest


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
          \ f -> isProvableZ modal f  == isProvableT modal f
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
          \ f -> all hasLeqTwoChildren $ proveZ modal f
        prop "tree for G3K" $
          \ f -> all hasLeqTwoChildren $ proveT modal f

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
          \ f g -> (isProvableZ modal f && isProvableZ modal g) <= isProvableZ modal (ConM f g)
        prop "tree for G3K" $
          \ f g -> (isProvableT modal f && isProvableT modal g) <= isProvableT modal (ConM f g)

    describe "If f isProvable in G3C, then neg neg f isProvable in G3I" $ do
        prop "zipper" $
          \ f -> isProvableZ classical f <= isProvableZ intui (negP (negP f))
        prop "tree" $
          \ f -> isProvableT classical f <= isProvableT intui (negP (negP f))
    describe "If f isProvable in G3C, then f isProvable in G3K" $ do
        prop "zipper" $
          \ f -> isProvableZ classical f <= isProvableZ modal (pTom f)
        prop "tree" $
          \ f -> isProvableT classical f <= isProvableT modal (pTom f)
