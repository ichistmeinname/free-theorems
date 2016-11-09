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

      getRelStr (RelVar _ (RVar s)) = "(RelVar " ++ s ++ ")"
      getRelStr (FunVar _ term) = "(FunVar " ++ (prettyTerm . fromEither $ term) ++ ")"
      getRelStr (RelBasic _) = "RelBasic"
      getRelStr (RelLift _ tc rels) = "(RelLift TC " ++ (unwords $ map getRelStr rels) ++ ")"
      getRelStr (RelFun _ r1 r2) = "(RelFun " ++ (getRelStr r1) ++ " " ++ (getRelStr r2) ++ ")"
      getRelStr (RelFunLab _ r1 r2) = "(RelFunLab " ++ (getRelStr r1) ++ " " ++ (getRelStr r2) ++ ")"
      getRelStr (RelAbs _ (RVar rv) (t1, t2) _ rel) = "(RelAbs \"" ++ rv ++ "\" (" ++ (show t1) ++ ", " ++ (show t2) ++ ") " ++ (getRelStr rel) ++ ")"
      getRelStr (RelTypeConsAbs _ _ (t1, t2) _ rel) = "(RelTypeConsAbs (" ++ (show t1) ++ ", " ++ (show t2) ++ ") " ++ (getRelStr rel) ++ ")"
      getRelStr (RelTypeConsApp _ (RVar rv) rel) = "(RelTypeConsApp \"" ++ rv ++ "\" " ++ (getRelStr rel) ++ ")"
      getRelStr (FunAbs _ _ (t1, t2) res rel) = "(FunAbs (" ++ (show t1) ++ ", " ++ (show t2) ++ ") " ++ (getRelStr rel) ++ ")"

      fromEither :: Either a a -> a
      fromEither (Left x) = x
      fromEither (Right x) = x

prettyTerm :: Term -> String
prettyTerm (TermVar (TVar s)) = s
prettyTerm (TermIns term ty) = (prettyTerm term) ++ "_{" ++ (show ty) ++ "}"
prettyTerm (TermApp t1 t2) = (prettyTerm t1) ++ " " ++ (prettyTerm t2)
prettyTerm (TermComp ts) = unwords (map prettyTerm ts)
