import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Theorems
import Language.Haskell.FreeTheorems.Parser.Hsx(parse)
import Language.Haskell.FreeTheorems.BasicSyntax
import Control.Monad(when)
import Control.Monad.Writer(Writer, runWriter, writer)
import Text.PrettyPrint.HughesPJ
import Data.List(intercalate)

import KnownDeclarations

data Gluku a = Gluku a

class Serializable a where
  serialize :: a b -> String

instance Serializable Gluku where
  serialize (Gluku x) = "hello"

class TestClass t where
  testtest :: t a -> String

hello :: (Functor f, TestClass f) => f a -> f a
hello = undefined

teststr :: String

describeFormula :: Formula -> String
describeFormula (ForallRelations (RVar rv) (t1, t2) rss f)
   = "(ForallRelations (RVar \"" ++ rv ++ "\") (\"" ++ (show t1) ++ "\", \"" ++ show t2 ++ "\")"
     ++ (intercalate "," $ map describeRestriction rss) ++ " " ++ describeFormula f ++ ")"
      -- ^ Quantifies a relation variable and two type expressions.

describeFormula (ForallFunctions tv (t1, t2) rss f)
   = let (TVar v) = either id id tv
      in "(ForallFunctions (TVar \"" ++ v ++ "\") (\"" ++ show t1 ++ "\", \"" ++ show t2 ++ "\") " ++
         (intercalate "," $ map describeRestriction rss) ++ " " ++ describeFormula f ++ ")"
      -- ^ Quantifies a function variable and two type expressions.

describeFormula (ForallTypeConstructors (RVar rv) (t1, t2) rss f)
   = "(ForallTypeConstructors \"" ++ rv ++ "\" (\"" ++ show t1 ++ "\", \"" ++ show t2 ++ "\") " ++
     (intercalate ", " $ map describeRestriction rss) ++ " " ++ describeFormula f ++ ")"

describeFormula (ForallPairs ((TVar t1), (TVar t2)) rel f)
   = "(ForallPairs (\"" ++ t1 ++ "\", \"" ++ t2 ++ "\") " ++ (show rel) ++ " " ++ describeFormula f ++ ")"

describeFormula (ForallVariables (TVar tv) te f)
   = "(ForallVariables (TVar \"" ++ tv ++ "\") \"" ++ show te ++ "\" " ++ describeFormula f ++ ")"


describeFormula (Equivalence f1 f2)
   = "(Equivalence " ++ describeFormula f1 ++ " " ++ describeFormula f2 ++ ")"

describeFormula (Implication f1 f2)
   = "(Implication " ++ describeFormula f1 ++ " " ++ describeFormula f2 ++ ")"

describeFormula (Conjunction f1 f2)
   = "(Conjunction " ++ describeFormula f1 ++ " " ++ describeFormula f2 ++ ")"

describeFormula (Predicate predi)
   = "(Predicate " ++ describePredicate predi ++ ")"


describeRestriction :: Restriction -> String
describeRestriction Strict           = "Strict"
describeRestriction Continuous       = "Continuous"
describeRestriction Total            = "Total"
describeRestriction BottomReflecting = "BottomReflecting"
describeRestriction LeftClosed       = "LeftClosed"
describeRestriction (RespectsClasses classes) = (intercalate ", ") . (map describeRespectsClass) $ classes

describeRespectsClass :: TypeClass -> String
describeRespectsClass (TC (Ident i)) = "(TC (Ident " ++ i ++ "))"

describePredicate :: Predicate -> String
describePredicate (IsMember t1 t2 rel)
   = "(IsMember \"" ++ show t1 ++ "\" \"" ++ show t2 ++ "\" " ++ show rel ++ ")"

describePredicate (IsEqual t1 t2)
   = "(IsEqual \"" ++ show t1 ++ "\" \"" ++ show t2 ++ "\")"

describePredicate (IsLessEq t1 t2)
  = "(IsLessEq \"" ++ show t1 ++ "\" \"" ++ show t2 ++ "\")"

describePredicate (IsNotBot t)
  = "(IsNotBot \"" ++ show t ++ "\")"

describePredicate IsTrue
  = "IsTrue"


--teststr = "class Serializable a where\n\
--  \serialize :: a -> String\n\n\
--  \test :: Serializable s => s -> s"

teststr = "class Test f where\n\
          \  fmap :: (a -> b) -> f a -> f b\n\n\
          \test :: Test f => f a -> f a"

specializeIntermediate :: Intermediate -> Intermediate
specializeIntermediate i = spec i (relationVariables i)
  where
    spec i' [] = i'
    spec i' (v:vs) = spec (specialise i' v) vs

main :: IO ()
main = do
  putStrLn ""
  let s = "free theorems generator"
  putStrLn s
  putStrLn $ replicate (length s)  '*'
  mainLoop

mainLoop :: IO ()
mainLoop = do
  putStrLn "\nInput:"
  inp <- getLine
  let s = case inp of
           "teststr" -> teststr
           otherwise -> inp

  if (s /= ":q")
    then do
          let (decls, parseErrs) = runWriter $ parse s
          let (valdecls, checkErrs) = runWriter $ checkAgainst knownDeclarations decls
          let allDecls = valdecls ++ knownDeclarations

          putStrLn (if (null (parseErrs ++ checkErrs)) then "Everything ok."
                                        else (show . hcat $ parseErrs ++ checkErrs))

          let sigs = filterSignatures valdecls
          if (null sigs) then putStrLn "No valid signature found."
                         else case interpret valdecls BasicSubset (head sigs) of
                           Nothing     -> return ()
                           (Just intm) -> do
                             let specIntm = specializeIntermediate intm
                             putStrLn "\nInput:"
                             putStrLn s
                             putStrLn "\nIntermediate representation:"
                             putStrLn $ show intm
                             putStrLn "\nTheorem (formula structure):"
                             putStrLn $ (describeFormula . simplify . asTheorem) intm
                             putStrLn "\nTheorem:"
                             putStrLn $ show (prettyTheorem [] $ (simplify . asTheorem) intm)
                             putStrLn "\nSpecialized intermediate:"
                             putStrLn $ show specIntm
                             putStrLn "\nSpecialized (formula structure):"
                             putStrLn $ (describeFormula . simplify . asTheorem) specIntm
                             putStrLn "\nSpecialized:"
                             putStrLn $ show (prettyTheorem [] $ (simplify . asTheorem) specIntm)
                             putStrLn "\nUnfolded lifts:"
                             let unflifts = unfoldLifts allDecls intm
                             putStrLn $ (show . hcat) $ map (prettyUnfoldedLift [] . simplifyUnfoldedLift) unflifts
                             putStrLn "\nUnfolded classes:"
                             let unfclasses = unfoldClasses allDecls intm
                             putStrLn $ (show . hcat) $ map (prettyUnfoldedClass []) unfclasses
          mainLoop
    else return ()
