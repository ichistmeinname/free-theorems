import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Theorems
import Language.Haskell.FreeTheorems.Parser.Hsx
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
teststr2 = "data T14 a19 a10 a13 a6\n\
    \  = D4 t2\n\
    \  | D10 !t2 !Int\n\
    \  | GT !T17\n\
    \  | D10 !T12\n\
    \  | D11 !t2 !a12 !t4\n\
    \  | Nothing !a13 t3 Int"

teststr3 :: String
teststr3 = "class Monad m where\n\
    \  (>>=) :: m a -> (a -> m b) -> m b\n"

test :: String -> Writer String (Maybe Intermediate)
test st = writer (interm sigs, errorStr)
            where
              (decls, parseErrs) = runWriter $ parse st
              (valdecls, checkErrs) = runWriter $ check decls
              errorStr = show $ hcat (parseErrs ++ checkErrs)
              sigs = filterSignatures valdecls
              interm []  = Nothing
              interm sgs = interpret valdecls BasicSubset (head sgs)

showSpecialisedList :: Intermediate -> [RelationVariable] -> IO ()
showSpecialisedList _ []     = return ()
showSpecialisedList t (r:rs) = (print (prettyTheorem [] (asTheorem (specialise t r)))) >> (showSpecialisedList t rs)

main :: IO ()
main = do
  putStr "Type signature: "
  x <- getLine
  let (intm, err) = runWriter $ test x
  putStrLn err
  case intm of
    Just t  -> do
                print (prettyTheorem [] (asTheorem t))
                showSpecialisedList t (relationVariables t)
    Nothing -> return ()
