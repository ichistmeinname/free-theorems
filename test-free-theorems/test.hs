import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Theorems
import Language.Haskell.FreeTheorems.Parser.Hsx
import Language.Haskell.FreeTheorems.BasicSyntax
import Control.Monad(when)
import Control.Monad.Writer(Writer, runWriter, writer)
import Text.PrettyPrint.HughesPJ

import KnownDeclarations

teststr :: String
teststr = "class Functor f where\n\n\
          \fmap :: Functor f => (a -> b) -> f a -> f b"
--          \test :: Functor f => f a -> f a"

specializeIntermediate :: Intermediate -> Intermediate
specializeIntermediate i = spec i (relationVariables i)
  where
    spec i [] = i
    spec i (v:vs) = spec (specialise i v) vs

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

  if (s /= "exit")
    then do
          let (decls, parseErrs) = runWriter $ parse s
          let (valdecls, checkErrs) = runWriter $ checkAgainst knownDeclarations decls

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
                             putStrLn $ show (prettyTheorem [] $ asTheorem intm)
                             putStrLn "\nSpecialized:"
                             let specIntm = specializeIntermediate intm
                             putStrLn $ show (prettyTheorem [] $ asTheorem specIntm)
                             putStrLn "\nUnfolded lifts:"
                             let unflifts = unfoldLifts valdecls intm
                             putStrLn $ (show . hcat) $ map (prettyUnfoldedLift []) unflifts
                             putStrLn "\nUnfolded classes:"
                             let unfclasses = unfoldClasses valdecls intm
                             putStrLn $ (show . hcat) $ map (prettyUnfoldedClass []) unfclasses
          mainLoop
    else return ()
