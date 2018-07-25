{-# LANGUAGE FlexibleInstances, Rank2Types #-}
module Main where

import Network.CGI
import Text.XHtml
import Data.List (intercalate, (\\))
import Data.Maybe (fromMaybe)
import Control.Monad (MonadPlus, mzero)

import Language.Haskell.FreeTheorems
import Language.Haskell.FreeTheorems.Theorems (Theorem)

import KnownDeclarations (knownDeclarations, arbitraryButFixedTypes)
import Pages (input_page, help_page)
import FTTools
import GeneratePDF (generatePDF, generateTeX)
import TypesetAsImage (typesetAsHtmlImg)
import LogRequests (logRequest)




main = runCGI (handleErrors cgiMain)

cgiMain :: CGI CGIResult
cgiMain = do
   mb_help <- getInput "help"
   case mb_help of
      Nothing -> cgiInputPage
      Just _  -> cgiHelpPage

cgiInputPage :: CGI CGIResult
cgiInputPage = do
   inputs <- getInputs
   logRequest inputs
   let generate = case lookup "format" inputs of
                     Nothing          -> generateHTML Graphical inputs
                     Just "html+png"  -> generateHTML Graphical inputs
                     Just "html+text" -> generateHTML PlainText inputs
                     Just "tex"       -> generateTeX
                     Just "pdf"       -> generatePDF
       model   = modelFromOptions inputs
       xtraSrc = fromMaybe "" $ lookup "xtraSrc" inputs
       --showTheorem = showTheoremFromOptions inputs
       outputHtml = output . renderHtml . input_page inputs
       reportError title text = outputHtml $ errorBox title text
   case lookup "type" inputs of
      Nothing -> outputHtml noHtml
      Just type_ ->
         if null type_
            then reportError "Error: Missing type" "Please enter a type in the input field."
            else case parseDeclarations knownDeclarations xtraSrc of
               Left err -> reportError "Error while parsing extra declarations" err
               Right xtraDecls ->
                  let decls = knownDeclarations ++ xtraDecls
                  in case parseTypeString decls type_ of
                     Left err -> reportError "Error while parsing type string" err
                     Right sig ->
                        case interpret decls model sig of
                           Nothing -> reportError "Error while interpreting signature" "not possible"
                           Just im -> generate model (showOptions inputs) decls sig im

modelFromOptions opts =
   case lookup "model" opts of
      Nothing      -> BasicSubset
      Just "basic" -> BasicSubset
      Just "fix"   -> SubsetWithFix style
      Just "seq"   -> SubsetWithSeq style
   where
      style = maybe EquationalTheorem styleFromOption $ lookup "style" opts
      styleFromOption "eq"   = EquationalTheorem
      styleFromOption "ineq" = InequationalTheorem

showOptions opts =
   case lookup "hideTypeInstantiations" opts of
      Just "yes" -> [OmitTypeInstantiations]
      Nothing    -> []


data FormulaeFormat = Graphical -- Formulae are rendered as graphics
                    | PlainText -- Formulae are rendered as plain text

generateHTML formulaeFormat inputs model showOptions decls sig im = do

  -- Select the appropriate rendering action
  let (renderTheorem, renderLifts, renderClasses) =
         case formulaeFormat of
            Graphical ->
               -- Convert to string and pass to the TeX renderer.
               ( liftIO . typesetAsHtmlImg      . (show . prettyTheorem showOptions) . asTheorem
               , liftIO . mapM typesetAsHtmlImg . map (show . prettyUnfoldedLift showOptions)
               , liftIO . mapM typesetAsHtmlImg . map (show . prettyUnfoldedClass showOptions)
               )
            PlainText ->
               -- Just convert everything to string.
               ( return . stringToHtml . (show . prettyTheorem showOptions) . asTheorem
               , return . map (stringToHtml . show . prettyUnfoldedLift showOptions)
               , return . map (stringToHtml . show . prettyUnfoldedClass showOptions)
               )

  -- Render all formulae to Html/[Html] with the selected method
  theorem            <- renderTheorem im
  special_theorem    <- renderTheorem special_im
  specialInv_theorem <- renderTheorem specialInv_im
  lifts              <- renderLifts   (unfoldLifts decls im)
  special_lifts      <- renderLifts   (unfoldLifts decls special_im)
  specialInv_lifts   <- renderLifts   (unfoldLifts decls specialInv_im)
  classes            <- renderClasses (unfoldClasses decls im)
  special_classes    <- renderClasses (unfoldClasses decls special_im)
  specialInv_classes <- renderClasses (unfoldClasses decls specialInv_im)

  -- Display the result page
  output $ renderHtml $ input_page inputs $
      thediv <<
         [ h3 << ("The Free Theorem for \"" ++ show sig ++ "\"")
         , pre << theorem
         , pre `aroundEach` lifts
         , pre `aroundEach` classes
         , h3 << "Reducing all permissable relation variables to functions"
         , pre << special_theorem
         , pre `aroundEach` special_lifts
         , pre `aroundEach` special_classes
         , concatHtml $ onlyIf (isInequational model)
            [ h3 << "Reducing all permissable relation variables to the inverse of functions"
            , pre << specialInv_theorem
            , pre `aroundEach` specialInv_lifts
            , pre `aroundEach` specialInv_classes
            ]
         ]
      where
         special_im    = specialiseAll im
         specialInv_im = specialiseAllInverse im



onlyIf :: MonadPlus m => Bool -> m a -> m a
onlyIf True  x = x
onlyIf False _ = mzero

aroundEach :: (HTML a) => (Html -> Html) -> [a] -> Html
aroundEach elem (x:xs) = elem (toHtml x) +++ aroundEach elem xs
aroundEach _    []     = noHtml

errorBox :: String -> String -> Html
errorBox title text =
   thediv ! [theclass "error"] <<
      [ h3 << title
      , p << text
      ]


cgiHelpPage :: CGI CGIResult
cgiHelpPage =
   output $ renderHtml $ help_page << html
      where html = map generateDeclarationBox
               [ ("Types of predefined functions", map show . filterSignatures)
               , ("Supported algebraic data types (excluding T0 to T9)", map rawDeclarationName . filterDataDeclarations)
               , ("Supported type synonyms", map rawDeclarationName . filterTypeDeclarations)
               , ("Supported type classes", map rawDeclarationName . filterClassDeclarations)
               ]
            generateDeclarationBox (text, f) =
               thediv <<
                  [ h3 << text
                  -- Not all known declarations are shown here, just the standard Haskell ones.
                  -- This excludes the arbitrary but fixed types T0 to T9
                  , pre << (unlines $ f $ knownDeclarations \\ arbitraryButFixedTypes)
                  ]

