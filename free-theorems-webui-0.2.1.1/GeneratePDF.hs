module GeneratePDF where

import Network.CGI
import System.FilePath (replaceExtension, (</>))
import System.Directory (removeFile)
import System.IO (openTempFile, hClose, hPutStr, getContents)
import System.Process (system)
import Data.List (intercalate)
import qualified Data.ByteString.Lazy as B

import Language.Haskell.FreeTheorems

import FTTools
import BeautifyTeXOutput (beautifyFormula)
import Paths (getTemporaryDirectory, getPathToAdditionalTeXFiles)



newtype PDF = PDF { fromPDF :: B.ByteString }

generatePDF model showOptions decls sig im = do
   setHeader "Content-type" "application/pdf"
   pdf <- liftIO (typeset $ asTeX model showOptions decls sig im)
   outputFPS (fromPDF pdf)

generateTeX model showOptions decls sig im = do
   setHeader "Content-type" "text/plain"
   output $ asTeX model showOptions decls sig im


asTeX model showOptions decls sig im =
  intercalate "\n" $
    [ "\\documentclass{article}"
    , "\\usepackage{amsmath}"
    , "\\pagestyle{empty}"
    , "\\parindent 0"
    , ""
    , "\\begin{document}"
    , "\\input{lambdaTeX}"
    , ""
    , "The theorem generated for functions of the type"
    , ""
    , formula (show sig)

    , "in the sublanguage of Haskell with " ++ show model ++ ", is:"
    , ""
    , formula (showTheorem (ifAllowed simplify $ asTheorem im))
    , formula (intercalate parBreak (map (showLift . ifAllowed simplifyUnfoldedLift) (unfoldLifts decls im)))
    , formula (intercalate parBreak (map showClass (unfoldClasses decls im)))

    , "Reducing all permissible relation variables to functions yields:"
    , ""
    , formula ((showTheorem  (ifAllowed simplify $ asTheorem im2)))
    , formula (intercalate parBreak (map (showLift . ifAllowed simplifyUnfoldedLift) (unfoldLifts decls im2)))
    , formula (intercalate parBreak (map showClass (unfoldClasses decls im2)))

    ] ++ (if isInequational model
             then [ "Instead reducing all permissible relation variables to the inverses of functions yields:"
                  , ""
                  , formula (showTheorem (ifAllowed simplify $ asTheorem im3))
                  , formula (intercalate parBreak (map (showLift . ifAllowed simplifyUnfoldedLift) (unfoldLifts decls im3)))
                  , formula (intercalate parBreak (map showClass (unfoldClasses decls im3)))
                  ]
             else []
         ) ++
    [ "\\end{document}"
    ]
    where
       -- Different show functions
       showTheorem = show . prettyTheorem showOptions
       showLift    = show . prettyUnfoldedLift showOptions
       showClass   = show . prettyUnfoldedClass showOptions
       -- Return the argument if simplifications are allowed
       ifAllowed smpl = case model of
                            BasicSubset -> smpl
                            SubsetWithFix _ -> smpl
                            _ -> id
       im2 = specialiseAll im
       im3 = specialiseAllInverse im
       parBreak = "\n\n"

       formula = unlines . map ("> "++) . lines . beautifyFormula


instance Show LanguageSubset where
  show BasicSubset = "with no bottoms"
  show (SubsetWithFix EquationalTheorem) = "with general recursion but no selective strictness, equational style"
  show (SubsetWithFix InequationalTheorem) = "with general recursion but no selective strictness, inequational style"
  show (SubsetWithSeq EquationalTheorem) = "with general recursion and selective strictness, equational style"
  show (SubsetWithSeq InequationalTheorem) = "with general recursion and selective strictness, inequational style"


typeset texSource = do
   tmp_dir <- liftIO getTemporaryDirectory
   (tempPath, tempHandle) <- openTempFile tmp_dir "temp.tex"
   let pathTo ext = replaceExtension tempPath ext

   hPutStr tempHandle texSource
   hClose tempHandle


   tex_dir <- getPathToAdditionalTeXFiles
   system $ "TEXINPUTS=\"" ++ tex_dir ++ ":$TEXINPUTS\" pdflatex -interaction=batchmode --output-dir=\"" ++ tmp_dir ++ "\" " ++ pathTo "tex" ++ " > /dev/null"
   removeFile $ pathTo "tex"
   removeFile $ pathTo "aux"
   removeFile $ pathTo "log"

   pdfData <- B.readFile $ pathTo "pdf"
   removeFile $ pathTo "pdf"

   return $ PDF pdfData
