{-# LANGUAGE DataKinds #-}
{-# LANGUAGE MonoLocalBinds #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Lib (benchmark) where

import Test.QuickCheck
import Criterion.Main
import Data.Proxy
import GHC.TypeLits
import Control.DeepSeq
import qualified Numeric.LinearAlgebra.Data as HM
import qualified Numeric.LinearAlgebra.HMatrix as HM
import qualified Data.Matrix as DM
import qualified LAoP.Matrix.Type as TM
import qualified Linear as LM
import qualified Linear.V as LM
import qualified Linear.Matrix as LM
import qualified Data.Vector as V
import LAoP.Utils hiding ((.))

randomMatrixHM :: Int -> Int -> Gen (HM.Matrix Double)
randomMatrixHM cols rows = do
  l <- vectorOf (cols * rows) arbitrary
  let lr = buildList l cols
      m  = HM.fromLists lr
  return m

randomMatrixDM :: Int -> Int -> Gen (DM.Matrix Double)
randomMatrixDM cols rows = do
  l <- vectorOf (cols * rows) arbitrary
  let lr = buildList l cols
      m  = DM.fromLists lr
  return m

randomMatrixTM :: forall a b . (TM.CountableDimensions a b, TM.FromListsN Double a b) => Gen (TM.Matrix Double a b)
randomMatrixTM = do
  let cols = fromInteger (natVal (Proxy :: Proxy (TM.Count a)))
      rows = fromInteger (natVal (Proxy :: Proxy (TM.Count b)))
  l <- vectorOf (cols * rows) arbitrary
  let lr = buildList l cols
      m  = TM.fromLists lr
  return m

randomMatrixLM :: forall a b . (KnownNat a, KnownNat b, LM.Dim a, LM.Dim b) => Gen (LM.V a (LM.V b Double))
randomMatrixLM = do
  let cols = fromInteger (natVal (Proxy :: Proxy a))
      rows = fromInteger (natVal (Proxy :: Proxy b))
  l <- vectorOf (cols * rows) (arbitrary :: Gen Double)
  let lr = buildList l cols
      v  = map (fromJust . LM.fromVector @b . V.fromList) lr
      (Just m)  = LM.fromVector @a (V.fromList v)
  return m

fromJust (Just a) = a

buildList [] _ = []
buildList l r  = take r l : buildList (drop r l) r

-- 
setupEnv1 = do
  -- HMatrix
  hm11 <- generate (resize 1 (randomMatrixHM 10 10))
  hm12 <- generate (resize 1 (randomMatrixHM 10 10))
  -- Matrix
  dm11 <- generate (resize 1 (randomMatrixDM 10 10))
  dm12 <- generate (resize 1 (randomMatrixDM 10 10))
  -- Linear
  lm11 <- generate (resize 1 (randomMatrixLM @10 @10))
  lm12 <- generate (resize 1 (randomMatrixLM @10 @10))
  -- Laop
  tm11 <- generate (resize 1 (randomMatrixTM @(Natural 0 10) @(Natural 0 10)))
  tm12 <- generate (resize 1 (randomMatrixTM @(Natural 0 10) @(Natural 0 10)))
  return (
   (hm11, hm12)
   ,(dm11, dm12)
   ,(lm11, lm12)
   ,(tm11, tm12)
         )

setupEnv2 = do
  -- HMatrix
  hm11 <- generate (resize 1 (randomMatrixHM 25 25))
  hm12 <- generate (resize 1 (randomMatrixHM 25 25))
  -- Matrix
  dm11 <- generate (resize 1 (randomMatrixDM 25 25))
  dm12 <- generate (resize 1 (randomMatrixDM 25 25))
  -- Linear
  lm11 <- generate (resize 1 (randomMatrixLM @25 @25))
  lm12 <- generate (resize 1 (randomMatrixLM @25 @25))
  -- Laop
  tm11 <- generate (resize 1 (randomMatrixTM @(Natural 0 25) @(Natural 0 25)))
  tm12 <- generate (resize 1 (randomMatrixTM @(Natural 0 25) @(Natural 0 25)))
  return (
   (hm11, hm12)
   ,(dm11, dm12)
   ,(lm11, lm12)
   ,(tm11, tm12)
         )

setupEnv3 = do
  -- HMatrix
  hm11 <- generate (resize 1 (randomMatrixHM 50 50))
  hm12 <- generate (resize 1 (randomMatrixHM 50 50))
  -- Matrix
  dm11 <- generate (resize 1 (randomMatrixDM 50 50))
  dm12 <- generate (resize 1 (randomMatrixDM 50 50))
  -- Linear
  lm11 <- generate (resize 1 (randomMatrixLM @50 @50))
  lm12 <- generate (resize 1 (randomMatrixLM @50 @50))
  -- Laop
  tm11 <- generate (resize 1 (randomMatrixTM @(Natural 0 50) @(Natural 0 50)))
  tm12 <- generate (resize 1 (randomMatrixTM @(Natural 0 50) @(Natural 0 50)))
  return (
   (hm11, hm12)
   ,(dm11, dm12)
   ,(lm11, lm12)
   ,(tm11, tm12)
         )

setupEnv4 = do
  -- HMatrix
  hm11 <- generate (resize 1 (randomMatrixHM 75 75))
  hm12 <- generate (resize 1 (randomMatrixHM 75 75))
  -- Matrix
  dm11 <- generate (resize 1 (randomMatrixDM 75 75))
  dm12 <- generate (resize 1 (randomMatrixDM 75 75))
  -- Linear
  lm11 <- generate (resize 1 (randomMatrixLM @75 @75))
  lm12 <- generate (resize 1 (randomMatrixLM @75 @75))
  -- Laop
  tm11 <- generate (resize 1 (randomMatrixTM @(Natural 0 75) @(Natural 0 75)))
  tm12 <- generate (resize 1 (randomMatrixTM @(Natural 0 75) @(Natural 0 75)))
  return (
   (hm11, hm12)
   ,(dm11, dm12)
   ,(lm11, lm12)
   ,(tm11, tm12)
         )

setupEnv5 = do
  -- HMatrix
  hm11 <- generate (resize 1 (randomMatrixHM 100 100))
  hm12 <- generate (resize 1 (randomMatrixHM 100 100))
  -- Matrix
  dm11 <- generate (resize 1 (randomMatrixDM 100 100))
  dm12 <- generate (resize 1 (randomMatrixDM 100 100))
  -- Linear
  lm11 <- generate (resize 1 (randomMatrixLM @100 @100))
  lm12 <- generate (resize 1 (randomMatrixLM @100 @100))
  -- Laop
  tm11 <- generate (resize 1 (randomMatrixTM @(Natural 0 100) @(Natural 0 100)))
  tm12 <- generate (resize 1 (randomMatrixTM @(Natural 0 100) @(Natural 0 100)))
  return (
   (hm11, hm12)
   ,(dm11, dm12)
   ,(lm11, lm12)
   ,(tm11, tm12)
         )

setupEnv6 = do
  -- HMatrix
  hm11 <- generate (resize 1 (randomMatrixHM 200 200))
  hm12 <- generate (resize 1 (randomMatrixHM 200 200))
  -- Matrix
  dm11 <- generate (resize 1 (randomMatrixDM 200 200))
  dm12 <- generate (resize 1 (randomMatrixDM 200 200))
  -- Linear
  lm11 <- generate (resize 1 (randomMatrixLM @200 @200))
  lm12 <- generate (resize 1 (randomMatrixLM @200 @200))
  -- Laop
  tm11 <- generate (resize 1 (randomMatrixTM @(Natural 0 200) @(Natural 0 200)))
  tm12 <- generate (resize 1 (randomMatrixTM @(Natural 0 200) @(Natural 0 200)))
  return (
   (hm11, hm12)
   ,(dm11, dm12)
   ,(lm11, lm12)
   ,(tm11, tm12)
         )

setupEnv7 = do
  -- HMatrix
  hm11 <- generate (resize 1 (randomMatrixHM 400 400))
  hm12 <- generate (resize 1 (randomMatrixHM 400 400))
  -- Matrix
  dm11 <- generate (resize 1 (randomMatrixDM 400 400))
  dm12 <- generate (resize 1 (randomMatrixDM 400 400))
  -- Linear
  lm11 <- generate (resize 1 (randomMatrixLM @400 @400))
  lm12 <- generate (resize 1 (randomMatrixLM @400 @400))
  -- Laop
  tm11 <- generate (resize 1 (randomMatrixTM @(Natural 0 400) @(Natural 0 400)))
  tm12 <- generate (resize 1 (randomMatrixTM @(Natural 0 400) @(Natural 0 400)))
  return (
   (hm11, hm12)
   ,(dm11, dm12)
   ,(lm11, lm12)
   ,(tm11, tm12)
         )

setupEnv8 = do
  -- HMatrix
  hm11 <- generate (resize 1 (randomMatrixHM 800 800))
  hm12 <- generate (resize 1 (randomMatrixHM 800 800))
  -- Matrix
  dm11 <- generate (resize 1 (randomMatrixDM 800 800))
  dm12 <- generate (resize 1 (randomMatrixDM 800 800))
  -- Linear
  lm11 <- generate (resize 1 (randomMatrixLM @800 @800))
  lm12 <- generate (resize 1 (randomMatrixLM @800 @800))
  -- Laop
  tm11 <- generate (resize 1 (randomMatrixTM @(Natural 0 800) @(Natural 0 800)))
  tm12 <- generate (resize 1 (randomMatrixTM @(Natural 0 800) @(Natural 0 800)))
  return (
   (hm11, hm12)
   ,(dm11, dm12)
   ,(lm11, lm12)
   ,(tm11, tm12)
         )

setupEnv9 = do
  -- HMatrix
  hm11 <- generate (resize 1 (randomMatrixHM 1600 1600))
  hm12 <- generate (resize 1 (randomMatrixHM 1600 1600))
  -- Matrix
  dm11 <- generate (resize 1 (randomMatrixDM 1600 1600))
  dm12 <- generate (resize 1 (randomMatrixDM 1600 1600))
  -- Linear
  lm11 <- generate (resize 1 (randomMatrixLM @1600 @1600))
  lm12 <- generate (resize 1 (randomMatrixLM @1600 @1600))
  -- Laop
  tm11 <- generate (resize 1 (randomMatrixTM @(Natural 0 1600) @(Natural 0 1600)))
  tm12 <- generate (resize 1 (randomMatrixTM @(Natural 0 1600) @(Natural 0 1600)))
  return (
   (hm11, hm12)
   ,(dm11, dm12)
   ,(lm11, lm12)
   ,(tm11, tm12)
         )

benchmark :: IO ()
benchmark = do
  print "Starting benchmarks..."
  defaultMain [
   env setupEnv1 $ \ ~((hm11, hm12), (dm11, dm12), (lm11, lm12), (tm11, tm12)) -> bgroup "Matrix Composition 1" [
   bgroup "10x10" [
     bench "hmatrix" $ nf (HM.mul hm11) hm12
   , bench "matrix" $ nf (DM.multStd2 dm11) dm12
   , bench "linear" $ nf (LM.!*! lm11) lm12
   , bench "laop" $ nf (TM.comp tm11) tm12
   ] ],
   env setupEnv2 $ \ ~((hm11, hm12), (dm11, dm12), (lm11, lm12), (tm11, tm12)) -> bgroup "Matrix Composition 2" [
   bgroup "25x25" [
     bench "hmatrix" $ nf (HM.mul hm11) hm12
   , bench "matrix" $ nf (DM.multStd2 dm11) dm12
   , bench "linear" $ nf (LM.!*! lm11) lm12
   , bench "laop" $ nf (TM.comp tm11) tm12
   ] ],
   env setupEnv3 $ \ ~((hm11, hm12), (dm11, dm12), (lm11, lm12), (tm11, tm12)) -> bgroup "Matrix Composition 3" [
   bgroup "50x50" [
     bench "hmatrix" $ nf (HM.mul hm11) hm12
   , bench "matrix" $ nf (DM.multStd2 dm11) dm12
   , bench "linear" $ nf (LM.!*! lm11) lm12
   , bench "laop" $ nf (TM.comp tm11) tm12
   ] ],
   env setupEnv4 $ \ ~((hm11, hm12), (dm11, dm12), (lm11, lm12), (tm11, tm12)) -> bgroup "Matrix Composition 4" [
   bgroup "75x75" [
     bench "hmatrix" $ nf (HM.mul hm11) hm12
   , bench "matrix" $ nf (DM.multStd2 dm11) dm12
   , bench "linear" $ nf (LM.!*! lm11) lm12
   , bench "laop" $ nf (TM.comp tm11) tm12
   ] ],
   env setupEnv5 $ \ ~((hm11, hm12), (dm11, dm12), (lm11, lm12), (tm11, tm12)) -> bgroup "Matrix Composition 5" [
   bgroup "100x100" [
     bench "hmatrix" $ nf (HM.mul hm11) hm12
   , bench "matrix" $ nf (DM.multStd2 dm11) dm12
   , bench "linear" $ nf (LM.!*! lm11) lm12
   , bench "laop" $ nf (TM.comp tm11) tm12
   ] ],
   env setupEnv6 $ \ ~((hm11, hm12), (dm11, dm12), (lm11, lm12), (tm11, tm12)) -> bgroup "Matrix Composition 6" [
   bgroup "200x200" [
     bench "hmatrix" $ nf (HM.mul hm11) hm12
   , bench "matrix" $ nf (DM.multStd2 dm11) dm12
   , bench "linear" $ nf (LM.!*! lm11) lm12
   , bench "laop" $ nf (TM.comp tm11) tm12
   ] ],
   env setupEnv7 $ \ ~((hm11, hm12), (dm11, dm12), (lm11, lm12), (tm11, tm12)) -> bgroup "Matrix Composition 7" [
   bgroup "400x400" [
     bench "hmatrix" $ nf (HM.mul hm11) hm12
   , bench "matrix" $ nf (DM.multStd2 dm11) dm12
   , bench "linear" $ nf (LM.!*! lm11) lm12
   , bench "laop" $ nf (TM.comp tm11) tm12
   ] ],
   env setupEnv8 $ \ ~((hm11, hm12), (dm11, dm12), (lm11, lm12), (tm11, tm12)) -> bgroup "Matrix Composition 8" [
   bgroup "800x800" [
     bench "hmatrix" $ nf (HM.mul hm11) hm12
   , bench "matrix" $ nf (DM.multStd2 dm11) dm12
   , bench "linear" $ nf (LM.!*! lm11) lm12
   , bench "laop" $ nf (TM.comp tm11) tm12
   ] ],
   env setupEnv9 $ \ ~((hm11, hm12), (dm11, dm12), (lm11, lm12), (tm11, tm12)) -> bgroup "Matrix Composition 9" [
   bgroup "1600x1600" [
     bench "hmatrix" $ nf (HM.mul hm11) hm12
   , bench "matrix" $ nf (DM.multStd2 dm11) dm12
   , bench "linear" $ nf (LM.!*! lm11) lm12
   , bench "laop" $ nf (TM.comp tm11) tm12
   ] ]
              ]
