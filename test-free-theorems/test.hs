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
teststr2 :: String
teststr2 = "class Monad m where\n\
    \  (>>=) :: m a -> (a -> m b) -> m b\n\
    \\n\
    \test :: Monad m => m a -> m a"

-- Parse the haskell-string, interpret the first occuring type signature and
-- return its Intermediate representation
test :: String -> Writer String (Maybe Intermediate)
test st = writer (interm sigs, errorStr)
            where
              (decls, parseErrs) = runWriter $ parse st
              (valdecls, checkErrs) = runWriter $ checkAgainst knownDeclarations decls
              errorStr = show $ hcat (parseErrs ++ checkErrs)
              sigs = filterSignatures valdecls
              interm []  = Nothing
              interm sgs = interpret valdecls BasicSubset (head sgs)

textToAST :: String -> [Declaration]
textToAST st = let (decls, _) = runWriter $ parse st
                in decls

showSpecialisedList :: Intermediate -> [RelationVariable] -> IO ()
showSpecialisedList _ []     = return ()
showSpecialisedList t (r:rs) = (print (prettyTheorem [] (asTheorem (specialise t r)))) >> (showSpecialisedList t rs)

spezializeIntermediate :: Intermediate -> Intermediate
spezializeIntermediate i = spec i (relationVariables i)
  where
    spec i [] = i
    spec i (v:vs) = spec (specialise i v) vs

main :: IO ()
main = do
  putStr "Type signature: "
  x' <- getLine
--  x <- return teststr
  x <- return (if x' == "teststr" then teststr else x')
  putStrLn "Input:"
  putStr ("\n" ++ x ++ "\n")
  let (intm, err) = runWriter $ test x
  when (x /= "exit")
    ((putStrLn err) >>
    case intm of
      Just t  -> do
                  putStrLn "Intermediate representation:"
                  print t
                  putStrLn "\nasTheorem:"
                  print (prettyTheorem [] (asTheorem t))
                  putStrLn "\nspecialized Intermediate representation:"
                  print (spezializeIntermediate t)
                  putStrLn "\nspecialized:"
                  print (prettyTheorem [] (asTheorem (spezializeIntermediate t)))
--                    showSpecialisedList t (relationVariables t)
      Nothing -> return ()
    >> putStrLn "" >> main)
