module Lib
    ( generatePdf
    ) where

import qualified Data.ByteString.Char8 as BS
import System.IO (withFile, withBinaryFile, IOMode(ReadMode, WriteMode))


header:: [String]
header = ["%PDF-1.4", "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj"]

generatePdf :: String -> String -> IO ()
generatePdf input output = do    
    contents <- withFile input ReadMode BS.hGetContents
    let textLines = reverse $ BS.lines contents        
        pdfText = BS.unlines $ map (\(i, l) -> BS.pack ("0 " ++ show (i*14) ++ " Td (" ++ BS.unpack l ++ ") Tj")) (zip [0..] textLines)
        pdf = BS.pack $ unlines
            (
                header ++ [  
             "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 >> endobj"
            , "3 0 obj << /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842] /Contents 4 0 R /Resources << /Font << /F1 << /Type /Font /Subtype /Type1 /BaseFont /Arial >> >> >> >> endobj"
            , "4 0 obj << /Length " ++ show ((BS.length pdfText) + (BS.length $ BS.pack "BT /F1 12 Tf 72 800 Td")) ++ " >> stream"
            , "BT /F1 12 Tf 72 800 Td"
            , BS.unpack pdfText
            , "ET"
            , "endstream endobj"
            , "xref"
            , "0 5"
            , "0000000000 65535 f "
            , "0000000010 00000 n "
            , "0000000061 00000 n "
            , "0000000178 00000 n "
            , "0000000400 00000 n "
            , "trailer << /Root 1 0 R /Size 5 >>"
            , "startxref"
            , "500"
            , "%%EOF"
            ])
    withBinaryFile output WriteMode $ \h -> BS.hPutStr h pdf


