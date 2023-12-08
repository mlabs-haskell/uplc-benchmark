module UplcBenchmark.Utils (ptryDecodeData, passert) where

ptryDecodeData ::
  forall (a :: PType) (s :: S).
  (PTryFrom PData (PAsData a), PIsData a) =>
  Term s PData ->
  Term s a
ptryDecodeData = pfromData . unTermCont . fmap fst . tcont . ptryFrom

passert ::
  forall (a :: PType) (s :: S).
  Term s PString ->
  Term s PBool ->
  Term s a ->
  Term s a
passert msg cond x = pif cond x $ ptraceError msg
