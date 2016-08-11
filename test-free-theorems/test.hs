import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Parser.Hsx
import Control.Monad.Writer
import Text.PrettyPrint.HughesPJ

test :: Doc
test = case sigs of
     (sig1 : _) -> prettyTheorem [] $ theo (interpret s BasicSubset (head sigs))
     []         -> hcat sec
  where
   s = (fst . runWriter . check) parsed
   (parsed, sec) = (runWriter . parse) "test :: [a] -> ![a]\ntest xs = xs"
   sigs = filterSignatures s
   theo (Just i) = asTheorem i
   theo Nothing  = error "Nothing"
