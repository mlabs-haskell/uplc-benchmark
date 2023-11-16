{-# OPTIONS_GHC -Wno-unused-do-bind #-}
{-# LANGUAGE GHC2021, OverloadedStrings, QualifiedDo, DataKinds #-}

module Main where

import Plutarch.Monadic qualified as P
import Data.Text.IO qualified as Text
import Plutarch.Prelude hiding (psingleton)
import Plutarch.Api.V1.Value (pvalueOf, padaSymbol, padaToken)
import Plutarch.Api.V2 (PValidator)
import LambdaBuffers.NftMarketplace.Plutarch (NftMarketplaceDatum (NftMarketplaceDatum))

pverifyData ::
  forall (a :: PType) (s :: S).
  PTryFrom PData (PAsData a) =>
  Term s PData ->
  Term s (PAsData a)
pverifyData = unTermCont . fmap fst . tcont . ptryFrom

passert ::
  forall (a :: PType) (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s a ->
  Term s a
passert msg cond x = pif cond x $ ptraceError msg

pdummyValidator :: ClosedTerm PValidator
pdummyValidator = plam $ \rawDatum _rawRedeemer _ctx -> P.do
  (NftMarketplaceDatum price _seller _cancelKey) <-
    pmatch $ pfromData $ pverifyData @NftMarketplaceDatum rawDatum

  passert "Test" (pvalueOf # pfromData price # padaSymbol # padaToken #== 42) 
  popaque $ pconstant ()

main :: IO ()
main = Text.putStrLn "Hello"
