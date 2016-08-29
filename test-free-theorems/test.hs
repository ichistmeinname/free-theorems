import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Parser.Hsx
import Control.Monad.Writer(runWriter)
import Text.PrettyPrint.HughesPJ

teststr :: String
teststr = "class (C9 c, Show c, C10 c) => Num c\n\
    \f2 :: !a19\n\
    \f18 :: t3\n\
    \(||) :: []\n\
    \uncurry :: e\n\
    \f10 :: t4\n\
    \f1 :: a9\n\
    \maybe :: t5\n\
    \succ :: t1\n\
    \f12 :: Either\n"

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
test2 st = parse st
