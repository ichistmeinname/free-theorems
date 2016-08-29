import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Parser.Hsx
import Control.Monad.Writer(runWriter)
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

test :: String -> Doc
test st = case sigs of
     (sig1 : _) -> (hcat sec) $$ (prettyTheorem [] $ theo (interpret s BasicSubset (head sigs)))
     []         -> hcat sec
  where
   s = (fst . runWriter . check) parsed
   (parsed, sec) = (runWriter . parse) st
   sigs = filterSignatures s
   theo (Just i) = asTheorem i
   theo Nothing  = error "Nothing"

--test2 :: String -> String
test2 st = let (decls, errs) = runWriter $ parse st
            in decls

main :: IO ()
main = do
  putStrLn "Source code:"
  putStrLn teststr3
  putStrLn "Pretty printed parsing result:"
  x <- return $ test2 teststr3
  putStrLn $ render (hcat (map (text.show) x))
