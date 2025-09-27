module Lib
  ( parseInput,
    generatePdf
  )
where


import qualified Data.ByteString.Char8 as BS
import System.IO
    ( IOMode(WriteMode), withBinaryFile )

import Text.Parsec (ParseError, eof, many, noneOf, parse, endOfLine, sepBy, (<|>), char, spaces, many1, letter, alphaNum, try)
import Text.ParserCombinators.Parsec (GenParser)

import Data.Map (Map, fromList, (!))

-- | Estado de la fuente para la generación del PDF.
data FontState = FontState {
    fontFamily :: String,
    fontSize   :: Int
} deriving (Eq)

-- | Un parámetro es un par clave-valor.
type Param = (String, String)

-- | Un comando tiene un nombre y una lista de parámetros.
data Command = Command String [Param]
  deriving (Show, Eq)

-- | El contenido de un documento puede ser texto o un comando.
data Content = TextLine String | CommandItem Command
  deriving (Show, Eq)

-- | El documento parseado es una lista de elementos de contenido.
data PDFValue = PDFDocument [Content]
  deriving (Show, Eq)

-- | Parsea un identificador (letras).
identifier :: GenParser Char st String
identifier = many1 letter

-- | Parsea un valor de parámetro (letras y números).
paramValue :: GenParser Char st String
paramValue = many1 alphaNum

-- | Parsea un parámetro con el formato "nombre = valor".
paramParser :: GenParser Char st Param
paramParser = do
    name <- identifier
    _ <- spaces *> char '=' <* spaces
    value <- paramValue
    return (name, value)

-- | Parsea un comando con el formato "\nombre[param1=val1, param2=val2]".
commandParser :: GenParser Char st Content
commandParser = try $ do
    _ <- char '\\'
    cmdName <- identifier
    _ <- spaces *> char '[' <* spaces
    params <- paramParser `sepBy` (spaces *> char ',' <* spaces)
    _ <- spaces *> char ']'
    return $ CommandItem (Command cmdName params)

-- | Parsea una línea de texto plano.
textLine :: GenParser Char st Content
textLine = TextLine <$> many (noneOf "\n\r")

-- | El parser principal que lee múltiples líneas y las une.
textParser :: GenParser Char st PDFValue
textParser = do
    -- Lee una lista de contenidos, que pueden ser comandos o texto.
    contents <- (commandParser <|> textLine) `sepBy` endOfLine
    eof -- Asegura que hemos consumido toda la entrada.
    return $ PDFDocument contents

-- | Lee un fichero, lo parsea y devuelve el resultado o un error.
parseInput :: FilePath -> IO (Either ParseError PDFValue)
parseInput inputFilePath = do
    contents <- readFile inputFilePath
    return $ parse textParser inputFilePath contents

-- | Genera un fichero PDF a partir de un PDFValue.
generatePdf :: PDFValue -> FilePath -> IO ()
generatePdf (PDFDocument contents) outputPath = do
    let -- 1. Procesar contenido y definir helpers
        initialFontState = FontState { fontFamily = "Helvetica", fontSize = 12 }
        initialFoldState = ([], initialFontState)
        (pdfTextCommands, _finalState) = foldl processContent initialFoldState contents
  
        
        pad10 n = let s = show n in replicate (10 - length s) '0' ++ s
        
        -- 2. Construir objetos PDF como ByteStrings
        initialFontCmd = BS.pack$ fontRef (fontFamily initialFontState) ++ " " ++ show (fontSize initialFontState) ++ " Tf"
        contentStream = BS.intercalate (BS.pack"\n") ([BS.pack"BT", initialFontCmd, BS.pack"12 TL", BS.pack"72 800 Td"] ++ pdfTextCommands ++ [BS.pack"ET"])
        contentLength = BS.length contentStream
        fontResources =
            "/F1 << /Type /Font /Subtype /Type1 /BaseFont /Helvetica /Encoding /WinAnsiEncoding >>" ++
            "/F2 << /Type /Font /Subtype /Type1 /BaseFont /Courier /Encoding /WinAnsiEncoding >>" ++
            "/F3 << /Type /Font /Subtype /Type1 /BaseFont /Times-Roman /Encoding /WinAnsiEncoding >>"
        pdfObjects =
          [ BS.pack "%PDF-1.4"
          , BS.pack "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj"
          , BS.pack "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 >> endobj"
          , BS.pack ("3 0 obj << /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842] /Contents 4 0 R /Resources << /Font << " ++ fontResources ++ " >> >> >> endobj")
          , BS.pack ("4 0 obj << /Length " ++ show contentLength ++ " >> stream")
          , contentStream
          , BS.pack "endstream endobj"
          ]
        
        -- 3. Calcular offsets y construir la tabla xref y el trailer
        objectOffsets = scanl (\acc obj -> acc + fromIntegral (BS.length obj) + 1) 0 pdfObjects
        xrefEntries =
          [ BS.pack "xref"
          , BS.pack "0 5"
          , BS.pack "0000000000 65535 f "
          ] ++ map (BS.pack . (++ " 00000 n ") . pad10) (init objectOffsets)
        trailerOffset = last objectOffsets
        trailer =
          [ BS.pack "trailer << /Root 1 0 R /Size 5 >>"
          , BS.pack "startxref"
          , BS.pack (show trailerOffset)
          , BS.pack "%%EOF"
          ]
        
        -- 4. Unir todas las partes
        
        pdfContent = BS.unlines (pdfObjects ++ xrefEntries ++ trailer)

    withBinaryFile outputPath WriteMode (`BS.hPutStr` pdfContent)
  where
    -- Itera sobre el contenido y genera los comandos PDF apropiados.
    -- El 'acc' (acumulador) es una tupla de (lista de comandos, tamaño de fuente actual).
    processContent (commands, fontState) content =
        case content of
            TextLine str                
                | otherwise -> (commands ++ [BS.concat [ BS.pack "(", BS.pack str, BS.pack ") Tj T*"]], fontState)
            CommandItem (Command name params) ->
                case name of
                    "font" -> handleFontCommand (commands, fontState) params
                    _      -> (commands, fontState) -- Ignora comandos no reconocidos

   

    fontRefMap :: Map String String
    fontRefMap = fromList [("Helvetica", "/F1"), ("Courier", "/F2"), ("Times-Roman", "/F3")]

    fontRef family = fontRefMap ! family -- Assumes family exists

    handleFontCommand (commands, initialFontState) params =
      let
        -- Procesa cada parámetro y actualiza el estado de la fuente.
        finalFontState = foldl updateFontState initialFontState params
        -- Genera el comando PDF si el estado de la fuente ha cambiado.
        (newCommands, newFontState)
           | finalFontState /= initialFontState =
              let newFontCmd = BS.pack $ fontRef (fontFamily finalFontState) ++ " " ++ show (fontSize finalFontState) ++ " Tf"
                  newLeadingCmd = BS.pack $ show (fontSize finalFontState) ++ " TL"
              in (commands ++ [newFontCmd, newLeadingCmd], finalFontState)
          | otherwise = (commands, initialFontState)
      in (newCommands, newFontState)

    updateFontState state ("family", newFamily)
      | newFamily `elem` ["Helvetica", "Courier", "Times-Roman"] = state { fontFamily = newFamily }
      | otherwise = state -- Ignora familias de fuente no soportadas
    updateFontState state ("size", sizeStr) =
      case reads sizeStr of
        [(newSize, "")] -> state { fontSize = newSize }
        _               -> state -- Ignora tamaños no válidos
    updateFontState state _ = state -- Ignora parámetros no reconocidos

generatePdf _ _ = putStrLn "Tipo de PDFValue no soportado para la generación."
