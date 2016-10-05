-- | Pretty printer for Intermediate data structure
--   It provides functions to transform Intermediates into documents.
--   Only used for testing purposes.

module Language.Haskell.FreeTheorems.PrettyIntermediate where



import Text.PrettyPrint

import Language.Haskell.FreeTheorems.Intermediate
import Language.Haskell.FreeTheorems.PrettyBase
import Language.Haskell.FreeTheorems.LanguageSubsets
import Language.Haskell.FreeTheorems.Theorems

instance Show Intermediate where
  show (Intermediate name l rel _ _ _ _) = "(Intermediate \"" ++ name ++ "\" " ++ langStr ++ " " ++ (getRelStr rel) ++ " ...)"
    where
      langStr = case l of
        BasicSubset -> "BasicSubset"
        SubsetWithFix _ -> "SubsetWithFix"
        SubsetWithSeq _ -> "SubsetWithSeq"

      getRelStr (RelVar _ _) = "RelVar"
      getRelStr (FunVar _ _) = "FunVar"
      getRelStr (RelBasic _) = "RelBasic"
      getRelStr (RelLift _ _ _) = "RelLift"
      getRelStr (RelFun _ r1 r2) = "(RelFun " ++ (getRelStr r1) ++ " " ++ (getRelStr r2) ++ ")"
      getRelStr (RelFunLab _ r1 r2) = "(RelFunLab " ++ (getRelStr r1) ++ " " ++ (getRelStr r2) ++ ")"
      getRelStr (RelAbs _ (RVar rv) (t1, t2) _ rel) = "(RelAbs \"" ++ rv ++ "\" (" ++ (show t1) ++ ", " ++ (show t2) ++ ") " ++ (getRelStr rel) ++ ")"
      getRelStr (RelTypeConsAbs _ _ _ _ _) = "RelTypeConsAbs"
      getRelStr (RelTypeConsApp _ (RVar rv) _ _) = "(RelTypeConsApp \"" ++ rv ++ "\")"
      getRelStr FunAbs{} = "FunAbs"
