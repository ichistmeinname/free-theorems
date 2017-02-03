module TypesetAsImage where

import System.FilePath (replaceExtension)
import System.Directory (removeFile)
import System.IO (openTempFile, hClose, hPutStr, getContents)
import System.Process (system)
import Data.List (intercalate)
import qualified Data.ByteString as BS
import qualified Codec.Binary.Base64 as Base64
import Text.XHtml

import System.IO.Unsafe (unsafeInterleaveIO)

import BeautifyTeXOutput (beautifyFormula)

import Paths (getTemporaryDirectory, getPathToAdditionalTeXFiles)


typesetAsHtmlImg formula = do
   pngImage <- typesetAsPNG formula


   return $ asHtml pngImage
 where
   asHtml pngImage =
     let
       -- Extract width and height from the PNG header
       header = BS.drop 16 pngImage
       w = valueOf $ BS.take 4 header
       h = valueOf $ BS.take 4 $ BS.drop 4 header
       valueOf = sum . zipWith (*) [256^3, 256^2, 256, 1]
                     . map fromEnum
                     . BS.unpack
     in
       image ! [ src $ "data:image/png;base64," ++ Base64.encode (BS.unpack pngImage)
               , alt formula
               , width  (show (w `div` 2))
               , height (show (h `div` 2))
               ]


typesetAsPNG :: String -> IO BS.ByteString
typesetAsPNG formula = unsafeInterleaveIO $ do
   tmp_dir <- getTemporaryDirectory
   (tempPath, tempHandle) <- openTempFile tmp_dir "temp.tex"
   let pathTo ext = replaceExtension tempPath ext

   let texSource = asTeX formula
   hPutStr tempHandle texSource
   hClose tempHandle

   tex_dir <- getPathToAdditionalTeXFiles
   system $ "TEXINPUTS=\"" ++ tex_dir ++ ":$TEXINPUTS\" latex -interaction=batchmode --output-dir=\"" ++ tmp_dir ++ "\" " ++ pathTo "tex" ++ " > /dev/null"
   removeFile $ pathTo "tex"
   removeFile $ pathTo "aux"
   removeFile $ pathTo "log"

   system $ "dvipng -x 2400 -T tight -z 9 -bg transparent -o " ++ pathTo "png" ++ " " ++ pathTo "dvi" ++ " > /dev/null"
   removeFile $ pathTo "dvi"

   pngImage <- BS.readFile $ pathTo "png"
   removeFile $ pathTo "png"

   return pngImage


asTeX formula =
  intercalate "\n" $
    [ "\\documentclass{article}"
    , "\\usepackage{amsmath}"
    , "\\pagestyle{empty}"
    , ""
    , "\\begin{document}"
    , "\\input{lambdaTeX}"
    , ""
    , unlines $ map ("> "++) $ lines $ beautifyFormula formula
    , ""
    , "\\end{document}"
    ]
