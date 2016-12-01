module BeautifyTeXOutput
   ( beautifyFormula
   ) where

import Data.List (foldl', isPrefixOf)


beautifyFormula :: String -> String

beautifyFormula =
   subscripts . dictReplace . unlines . map cups . lines

dictReplace :: String -> String
dictReplace s = foldl' f s dictionary
   where f input (text, subst) = replace text subst input


replace :: String -> String -> String -> String
replace text subst = go
   where go input | text `isPrefixOf` input =
            subst ++ go (drop (length text) input)
         go (x:xs) = x : go xs
         go [] = []


-- The following is stuff by Florian Stenger

-- The left-hand sides of tuples are to be replaced by
-- the corresponding right-hand sides.
dictionary =
   [ ("->^{seq,=}", "relseqeq")
   , ("->^{seq,[=}", "relseqineq")
   , ("^{-1}", " invfun")
   , ("_|_", "undefined")
   , ("[=", "blw")
   ]

-- If a line starts with "u", replace "u" by "cup". Preceeding spaces are ignored.
cups :: String -> String
cups (' ':xs)     = " " ++ cups xs
cups ('u':' ':xs) = "cup " ++ xs
cups s            = s

-- Makes subscripts a little prettier by removing curly braces.
subscripts :: String -> String
subscripts [] = []
subscripts ('_':'{':xs) =
   '_' : subscripts' xs
subscripts (x:xs) =
   x : subscripts xs

subscripts' :: String -> String
subscripts' ('}':xs) = subscripts xs
subscripts' (x:xs) =
   x : subscripts' xs
