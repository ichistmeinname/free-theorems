import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Theorems
import Language.Haskell.FreeTheorems.Parser.Hsx
import Language.Haskell.FreeTheorems.BasicSyntax
import Language.Haskell.FreeTheorems.ValidSyntax
import Control.Monad(when)
import Control.Monad.Writer(Writer, runWriter, writer)
import Data.List
import Text.PrettyPrint.HughesPJ

describeValDecl :: ValidDeclaration -> String
describeValDecl d = "(ValidDeclaration " ++
                  describeDecl (rawDeclaration d) ++
                  " " ++
                  if (isStrictDeclaration d)
                    then "True"
                    else "False"
                  ++ ")"

describeDecl :: Declaration -> String
describeDecl (TypeDecl _) = "TypeDeclaration"
describeDecl (ClassDecl cl) = "(ClassDecl " ++ describeClass cl ++ ")"
describeDecl (TypeSig sig) = "(TypeSig " ++ describeSignature sig ++ ")"
describeDecl _ = "Unknown"

describeClass :: ClassDeclaration -> String
describeClass (Class tyCl i tv sigs) = "(Class {" ++
  (intercalate ", "
  ["superClasses = [" ++ (intercalate ", " $ map describeTypeClass tyCl) ++ "]",
   "className = " ++ describeIdentifier i,
   "classVar = " ++ describeTypeVariable tv,
   "classFuns = [" ++ (intercalate ", " $ map describeSignature sigs) ++ "]"])
  ++ "})"

describeTypeClass :: TypeClass -> String
describeTypeClass (TC i) = "(TypeClass " ++ (describeIdentifier i) ++ ")"

describeIdentifier :: Identifier -> String
describeIdentifier (Ident i) = "(Ident {unpackIdent = \"" ++ i ++ "\"})"

describeTypeVariable :: TypeVariable -> String
describeTypeVariable (TV i) = "(TV " ++ describeIdentifier i ++ ")"

describeSignature :: Signature -> String
describeSignature (Signature i e) = "(Signature {" ++
  intercalate ", "
    ["signatureName = " ++ describeIdentifier i,
     "signatureType = " ++ describeTypeExpression e]
  ++ "})"

describeTypeExpression :: TypeExpression -> String
describeTypeExpression (TypeVar tv) = "(TypeVar " ++ describeTypeVariable tv ++ ")"
describeTypeExpression (TypeCon tycon exps) = "(TypeCon " ++ describeTypeConstructor tycon ++ ", [" ++
                                             (intercalate ", " $ map describeTypeExpression exps) ++
                                             "])"
describeTypeExpression (TypeFun e1 e2) = "(TypeFun " ++ describeTypeExpression e1 ++ " " ++
                                         describeTypeExpression e2 ++ ")"
describeTypeExpression (TypeFunLab e1 e2) = "(TypeFunLab " ++ describeTypeExpression e1 ++ " " ++
                                        describeTypeExpression e2 ++ ")"
describeTypeExpression (TypeAbs tv tycl e) = "(TypeAbs " ++ describeTypeVariable tv ++ " [" ++
                                        (intercalate ", " $ map describeTypeClass tycl) ++
                                        "] " ++ describeTypeExpression e ++ ")"
describeTypeExpression (TypeAbsLab tv tycl e) = "(TypeAbsLab " ++ describeTypeVariable tv ++ " [" ++
                                        (intercalate ", " $ map describeTypeClass tycl) ++
                                        "] " ++ describeTypeExpression e ++ ")"
describeTypeExpression (TypeExp _) = "TypeExp"
describeTypeExpression (TypeVarApp tv exps) = "(TypeVarApp " ++ describeTypeVariable tv ++ " [" ++
                                              (intercalate ", " $ map describeTypeExpression exps) ++"])"

describeTypeConstructor :: TypeConstructor -> String
describeTypeConstructor _ = "TypeConstructor"

main :: IO ()
main = do
  putStrLn "Type the declaration you want to convert to ValidDeclaration:"
  x <- getLine
  let (decls, docs) = runWriter $ parse x
  if (not . null $ docs)
    then putStrLn (show $ hcat docs)
    else do
      let (valdecls, docs') = runWriter $ check decls
      if (not . null $ docs')
        then putStrLn (show $ hcat docs')
        else putStrLn (concat $ map describeValDecl valdecls)
