module UplcBenchmark.Utils (pverifyData, passert) where

pverifyData ::
  forall (a :: PType) (s :: S).
  (PTryFrom PData (PAsData a)) =>
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
