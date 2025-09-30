import Lib (parseInput, generatePdf)
import System.Environment (getArgs)

main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> do
            putStrLn $ "Parsing " ++ inputFile ++ "..."
            parseResult <- parseInput inputFile
            case parseResult of
                Left err -> do
                    putStrLn "Error parsing file:"
                    print err
                Right pdfValue -> do
                    putStrLn "Parse successful. Generating PDF..."
                    generatePdf pdfValue outputFile
                    putStrLn $ outputFile ++ " generated."
        _ -> putStrLn "Usage: topdf-exe <input-file> <output-file>"