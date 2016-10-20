import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Theorems
import Language.Haskell.FreeTheorems.Parser.Hsx
import Language.Haskell.FreeTheorems.BasicSyntax
import Control.Monad(when)
import Control.Monad.Writer(Writer, runWriter, writer)
import Text.PrettyPrint.HughesPJ

teststr :: String
teststr = "class Monad m where\n\
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
--                    showSpecialisedList t (relationVariables t)
      Nothing -> return ()
    >> putStrLn "" >> main)
