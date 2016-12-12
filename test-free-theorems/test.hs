import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Theorems
import Language.Haskell.FreeTheorems.Parser.Hsx(parse)
import Language.Haskell.FreeTheorems.BasicSyntax
import Control.Monad(when)
import Control.Monad.Writer(Writer, runWriter, writer)
import Text.PrettyPrint.HughesPJ

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
                             putStrLn "\nInput:"
                             putStrLn s
                             putStrLn "\nIntermediate representation:"
                             putStrLn (show intm)
                             putStrLn "\nTheorem:"
                             putStrLn $ show (prettyTheorem [] $ (simplify . asTheorem) intm)
                             putStrLn "\nSpecialized:"
                             let specIntm = specializeIntermediate intm
                             putStrLn $ show (prettyTheorem [] $ (simplify . asTheorem) specIntm)
                             putStrLn "\nUnfolded lifts:"
                             let unflifts = unfoldLifts allDecls intm
                             putStrLn $ (show . hcat) $ map (prettyUnfoldedLift [] . simplifyUnfoldedLift) unflifts
                             putStrLn "\nUnfolded classes:"
                             let unfclasses = unfoldClasses allDecls intm
                             putStrLn $ (show . hcat) $ map (prettyUnfoldedClass []) unfclasses
          mainLoop
    else return ()
