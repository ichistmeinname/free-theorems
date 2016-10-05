import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Theorems
import Language.Haskell.FreeTheorems.Parser.Hsx
import Language.Haskell.FreeTheorems.BasicSyntax
import Control.Monad.Writer(Writer, runWriter, writer)
import Text.PrettyPrint.HughesPJ

teststr :: String
teststr = "class (C9 c, Show c, C10 c) => Num c where\n\
    \  f2 :: !a19\n\
    \  f18 :: t3\n\
    \  (||) :: []\n\
    \  uncurry :: e\n\
    \  f10 :: t4\n\
    \  f1 :: a9\n\
    \  maybe :: t5\n\
    \  succ :: t1\n\
    \  f12 :: Either\n"

teststr2 :: String
teststr2 = "data Test a\n\
    \  = Test a"

teststr3 :: String
teststr3 = "class Monad m where\n\
    \  (>>=) :: m a -> (a -> m b) -> m b\n\
    \\n\
    \test :: Monad m => m a -> m a"

-- Parse the haskell-string, interpret the first occuring type signature and
-- return its Intermediate representation
test :: String -> Writer String (Maybe Intermediate)
test st = writer (interm sigs, errorStr)
            where
              (decls, parseErrs) = runWriter $ parse st
              (valdecls, checkErrs) = runWriter $ check decls
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

-- Problem: Muss Intermediate selbst darstellen, da keine Show-instanz

main :: IO ()
main = do
  putStr "Type signature: "
--  x <- getLine
  x <- return teststr3
  let (intm, err) = runWriter $ test x
  putStrLn err
  case intm of
    Just t  -> do
                  print t
--                print (prettyTheorem [] (asTheorem t))
--                showSpecialisedList t (relationVariables t)
    Nothing -> return ()
