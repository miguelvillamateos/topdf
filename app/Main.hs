
import Lib (parseInput, generatePdf)

main :: IO ()
main = do
    putStrLn "Parsing input.txt..."
    parseResult <- parseInput "input.txt"
    case parseResult of
        Left err -> do
            putStrLn "Error parsing file:"
            print err
        Right pdfValue -> do
            putStrLn "Parse successful. Generating PDF..."
            generatePdf pdfValue "output.pdf"
            putStrLn "output.pdf generated."