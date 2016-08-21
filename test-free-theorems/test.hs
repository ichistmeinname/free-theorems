import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Parser.Hsx
import Control.Monad.Writer
import Text.PrettyPrint.HughesPJ

teststr :: String
teststr = "data Test a = ConstTest a\n" ++
          "test :: [Test a] -> [a]\n" ++
          "test xs = xs"

test :: String -> Doc
test st = case sigs of
     (sig1 : _) -> prettyTheorem [] $ theo (interpret s BasicSubset (head sigs))
     []         -> hcat sec
  where
   s = (fst . runWriter . check) parsed
   (parsed, sec) = (runWriter . parse) st
   sigs = filterSignatures s
   theo (Just i) = asTheorem i
   theo Nothing  = error "Nothing"
