import Lib (parseInput, generatePdf)
import System.Environment (getArgs)
import System.IO (IOMode(AppendMode), withFile)
import Control.Exception (try)


main :: IO ()
main = do
    args <- getArgs
    case args of
        [inputFile, outputFile] -> do
            -- Intenta abrir el fichero de salida para comprobar si se puede escribir.
            writePermission <- try (withFile outputFile AppendMode (\_ -> return ())) :: IO (Either IOError ())
            case writePermission of
                Left e ->
                    putStrLn $ "Error: Cannot write to output file '" ++ outputFile ++ "'.\n" ++
                               "It might be locked by another program or you may not have permissions.\n" ++
                               "Error details: " ++ show e
                Right () -> do
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