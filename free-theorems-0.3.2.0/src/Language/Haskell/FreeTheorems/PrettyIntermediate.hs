-- | Pretty printer for Intermediate data structure
--   It provides functions to transform Intermediates into documents.
--   Only used for testing purposes.

module Language.Haskell.FreeTheorems.PrettyIntermediate where



import Text.PrettyPrint

import Language.Haskell.FreeTheorems.BasicSyntax
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

instance Show Relation where
  show = getRelStr

instance Show Term where
  show = prettyTerm

getRelStr :: Relation -> String
getRelStr (RelVar ri (RVar s)) = "(RelVar " ++ getRelInfStr ri ++ " \"" ++ s ++ "\")"
getRelStr (FunVar ri term) = "(FunVar " ++ getRelInfStr ri ++ " \"" ++ (prettyTerm . fromEither $ term) ++ "\")"
getRelStr (RelBasic ri) = "(RelBasic" ++ getRelInfStr ri ++ ")"
getRelStr (RelLift ri tc rels) = "(RelLift " ++ getRelInfStr ri ++ " (\"" ++ (show $ relationLeftType ri) ++ "\", \"" ++ (show $ relationRightType ri) ++ "\") " ++ getTypeConsStr tc ++ " " ++ (unwords $ map getRelStr rels) ++ ")"
getRelStr (RelFun ri r1 r2) = "(RelFun " ++ getRelInfStr ri ++ " " ++ (getRelStr r1) ++ " " ++ (getRelStr r2) ++ ")"
getRelStr (RelFunLab ri r1 r2) = "(RelFunLab " ++ getRelInfStr ri ++ " " ++ (getRelStr r1) ++ " " ++ (getRelStr r2) ++ ")"
getRelStr (RelAbs ri (RVar rv) (t1, t2) _ rel) = "(RelAbs " ++ getRelInfStr ri ++ " \"" ++ rv ++ "\" (\"" ++ (show t1) ++ "\", \"" ++ (show t2) ++ "\") " ++ (getRelStr rel) ++ ")"
getRelStr (RelTypeConsAbs ri _ (t1, t2) _ rel) = "(RelTypeConsAbs " ++ getRelInfStr ri ++ " (\"" ++ (show t1) ++ "\", \"" ++ (show t2) ++ "\") " ++ (getRelStr rel) ++ ")"
getRelStr (RelTypeConsApp ri (RVar rv) rel) = "(RelTypeConsApp " ++ getRelInfStr ri ++ " \""
                                        ++ rv ++ "\" " ++ (getRelStr rel) ++ ")"
getRelStr (FunAbs ri n (t1, t2) res rel) = let (TVar v) = fromEither n
                                           in "(FunAbs " ++ getRelInfStr ri ++ " \"" ++ v ++ "\" (\"" ++ (show t1) ++ "\", \"" ++ (show t2) ++ "\") " ++ (getRelStr rel) ++ ")"

getRelInfStr :: RelationInfo -> String
getRelInfStr (RelationInfo _ t1 t2) = "(RelationInfo _ \"" ++ show t1 ++ "\", \"" ++ show t2 ++ "\")"

fromEither :: Either a a -> a
fromEither = either id id

getTypeConsStr :: TypeConstructor -> String
getTypeConsStr ConUnit            = "ConUnit"
getTypeConsStr ConList            = "ConList"
getTypeConsStr (ConTuple i)       = "(ConTuple " ++ show i ++ ")"
getTypeConsStr ConInt             = "ConInt"
getTypeConsStr ConInteger         = "ConInteger"
getTypeConsStr ConFloat           = "ConFloat"
getTypeConsStr ConDouble          = "ConDouble"
getTypeConsStr ConChar            = "ConChar"
getTypeConsStr (Con (Ident i))    = "(Con (Ident \"" ++ i ++ "\"))"
getTypeConsStr (ConVar (Ident i)) = "(ConVar (Ident \"" ++ i ++ "\"))"

prettyTerm :: Term -> String
prettyTerm (TermVar (TVar s)) = s
prettyTerm (TermIns term ty) = (prettyTerm term) ++ "_{" ++ (show ty) ++ "}"
prettyTerm (TermApp t1 t2) = (prettyTerm t1) ++ " " ++ (prettyTerm t2)
prettyTerm (TermComp ts) = unwords (map prettyTerm ts)
