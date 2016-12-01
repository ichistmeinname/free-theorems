module Pages (input_page, help_page) where

import Text.XHtml
import Data.Maybe (fromMaybe, isJust)

import Paths (relativeURLOf)

-- The hackage URL of a package
hackage :: String -> URL
hackage pkgName = "http://hackage.haskell.org/cgi-bin/hackage-scripts/package/" ++ pkgName

-- Link text to url
at :: (HTML a) => a -> URL -> HotLink
text `at` url = hotlink url << text


page_template title_text content =
   thehtml <<
      [ header <<
         [ thetitle << title_text
         , thelink ! [rel "stylesheet", thetype "text/css", href (relativeURLOf "style.css")] << ""
         , script ! [src "http://code.jquery.com/jquery-1.4.2.min.js", thetype "text/javascript"] << ""
         , script ! [src (relativeURLOf "ui-tweaks.js"), thetype "text/javascript"] << ""
         ]
      , body <<
         [ thediv ! [identifier "header"] <<
            [ h1 << "Haskell"
            , h2 << title_text
            ]
         , thediv ! [identifier "content"] << content
         ]
      ]


input_page :: [(String, String)] -> Html -> Html
input_page inputs additionalContent =
   let title_text = "Automatic generation of free theorems"
   in page_template title_text <<
         [ p ! [identifier "help"] << "Help" `at` "?help"
         , thediv <<
            [ p << ("This tool allows to generate free theorems for sublanguages of Haskell. See the " +++ "help page" `at` "?help"  +++ " for details.")
            , p << ("The source is available at hackage as " +++ ("free-theorems-webui" `at` hackage "free-theorems-webui") +++ ". See also: " +++ ("free-theorems (library)" `at` hackage "free-theorems") +++ " and " +++ ("ftshell (shell interface)" `at` hackage "ftshell") +++ ".")
            , p << ("You may also want to try the following related tools:" +++ ulist << [ li << "Automatically Generating Counterexamples to Naive Free Theorems" `at` "http://www-ps.iai.uni-bonn.de/cgi-bin/exfind.cgi"
                                                                                         , li << "Taming Selective Strictness" `at` "http://www-ps.iai.uni-bonn.de/cgi-bin/polyseq.cgi"
                                                                                         ])
            ]
         , thediv <<
            form ! [method "POST", theclass "float-container"] <<
               [ thediv <<
                  [ p << "Please enter a (polymorphic) type, e.g. \"(a -> Bool) -> [a] -> [a]\" or simply \"filter\":"
                  , p << textfield' "type" ! [theclass "type", size "50", maxlength 1024]
                  , p << "Please choose a sublanguage of Haskell:"
                  , p << (check radioOption' { o_name = "model", o_value = "basic", o_label = "no bottoms (hence no general recursion and no selective strictness)" })
                  , p << (      radioOption' { o_name = "model", o_value = "fix"  , o_label = "general recursion but no selective strictness" })
                  , p << (      radioOption' { o_name = "model", o_value = "seq"  , o_label = "general recursion and selective strictness" })
                  , p << "Please choose a theorem style (without effect in the sublanguage with no bottoms):"
                  , p << (check radioOption' { o_name = "style", o_value = "eq"   , o_label = "equational" })
                  , p << (      radioOption' { o_name = "style", o_value = "ineq" , o_label = "inequational" })
                  ]
               , thediv <<
                  [ p << "If you need additional declarations you can enter them here:"
                  , p << textarea' "xtraSrc"
                  ]
               , thediv ! [theclass "clear"] <<
                  [ submit "" "Generate"
                  , thespan <<
                     yesNoOption' { o_name = "hideTypeInstantiations", o_value = "yes", o_label = "hide type instantiations", o_title = "Hide type instantiations in theorems for better readability." }
                  , thespan <<
                     [ check radioOption' { o_name = "format", o_value = "html+png" , o_label = "PNG"  , o_title = "Show result with graphical formulae"  }
                     ,       radioOption' { o_name = "format", o_value = "html+text", o_label = "Plain", o_title = "Show result with plain text formulae" }
                     ,       radioOption' { o_name = "format", o_value = "tex"      , o_label = "TeX"  , o_title = "Export as TeX" }
                     ,       radioOption' { o_name = "format", o_value = "pdf"      , o_label = "PDF"  , o_title = "Export as PDF" }
                     ] +++ " " +++ ("?" `at` "?help#format" ! [title "Help on output formats"])
                  ]
               ]
         , additionalContent
         ]
   where
      -- Define versions of all used input elements which
      -- implicitly use the values from the "inputs" parameter.
      -- This causes input elements to keep their values after form submission,
      -- provided the result from getInputs is passed to this function.
      radioOption' = radioOption { o_inputs = inputs }
      yesNoOption' = yesNoOption { o_inputs = inputs }
      textfield' name_ = (! [value $ fromMaybe "" $ lookup name_ inputs]) textfield name_
      textarea'  name_ = textarea ! [name name_] << (fromMaybe "" $ lookup name_ inputs)


data Option = Option { o_type :: String, o_name :: String, o_value :: String, o_label :: String, o_title :: String, o_checked :: Bool, o_inputs :: [(String, String)] }

check :: Option -> Option
check opt = opt { o_checked = True }

instance HTML Option where
   toHtml (Option tp n v l t c inputs) =
      toHtml (input ! attributes +++ label ! [thefor input_id] << l)
         where attributes = [thetype tp, name n, value v, title t, identifier input_id] ++ if maybe c (v==) (lookup n inputs) then [checked] else []
               input_id   = n ++ "_" ++ v

yesNoOption = Option "checkbox" "" "" "" "" False []
radioOption = Option "radio"    "" "" "" "" False []



help_page :: Html -> Html
help_page additionalContent =
   let title_text = "Help on: Automatic generation of free theorems"
   in page_template title_text <<
         [ thediv << ("This is the help page for the " +++ "Free Theorem Generator" `at` "?" +++ ".")
         , thediv <<
            [ h3 << "Free Theorems"
            , p << ("Free Theorems were first described in the paper " +++ "\"Theorems for free!\"" `at` "http://doi.acm.org/10.1145/99370.99404" +++ " by Philip Wadler. " +++
                    "Their special property is that they can be derived solely from the type of a function. The key idea is to interpret types as relations. " +++
                    "To reflect the structure of types, a relational action, which maps relations to a relation, is defined for every type constructor. " +++
                    "Using relational actions, a relation can be constructed for every type, and, by applying the definitions of these relational actions, free theorems are obtained.")
            , p << ("General recursion and selective strictness weaken free theorems. " +++
                    "To show the influences caused by adding these constructs, several sublanguages of Haskell are supported in this tool. " +++
                    "The theoretical foundations are described in the paper " +++ "\"Free theorems in the presence of seq\"" `at` "http://doi.acm.org/10.1145/982962.964010" +++ " by Patricia Johann and Janis Voigtländer. " +++
                    "As motivated there, it is possible to derive both equational and inequational free theorems.")
            ]
         , thediv <<
            [ h3 << "Types"
            , p << [ thespan << "Types can be entered in two ways, either with a name or without one. Examples would be "
                   , pre << "g :: forall b . (Int -> b -> b) -> b -> b"
                   , thespan << " or "
                   , pre << "[a] -> [a]"
                   ]
            , p << "It is also possible to simply enter the name of a predefined Haskell function (see the list given below). The tool knows about standard Haskell data types, type synonyms, and type classes (again, see the lists below)."
            , p << "Additionally, T0 to T9 may be used as placeholders for arbitrary but fixed types."
            ]
         , anchor ! [ identifier "format" ] << ""
         , thediv <<
            [ h3 << "Output formats"
            , p << "The following output formats can be selected:"
            , defList
                 [ ("PNG",  "Result is displayed on webpage, formulae are rendered as PNG images — " +++ emphasize << "This is the default.")
                 , ("Plain", stringToHtml "Result is displayed on webpage, formulae are rendered as plain text.")
                 , ("TeX",  "A TeX file is generated. You need " +++ "lambdaTeX.tex" `at` relativeURLOf "lambdaTeX.tex" +++ " (adapted from Patryk Zadarnowski) and pdflatex to get the same result as by selecting the 'PDF' option.")
                 , ("PDF",   stringToHtml "A PDF file is generated.")
                 ]
            ]
         , additionalContent
         ]
