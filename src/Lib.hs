module Lib
  ( parseInput,
    generatePdf,
  )
where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8
import Data.Map (Map, fromList, toList, (!))
import System.IO 
  ( IOMode (ReadMode, WriteMode),
    withFile,
    hGetContents,
    hSetEncoding,
    withBinaryFile,
    utf8
  )
import Text.Parsec (ParseError, alphaNum, char, endOfLine, eof, letter, many, many1, noneOf, parse, sepBy, spaces, try, (<|>))
import Text.ParserCombinators.Parsec (GenParser)
import Data.Word (Word8)

-- | Estado de la fuente para la generación del PDF.
data FontState = FontState
  { fontFamily :: String,
    fontSize :: Int
  }
  deriving (Eq)

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
parseInput inputFilePath = withFile inputFilePath ReadMode $ \handle -> do
  -- Establece explícitamente la codificación a UTF-8 para la lectura del fichero.
  hSetEncoding handle utf8
  contents <- hGetContents handle
  -- `parse` necesita que la entrada sea evaluada completamente.
  return $! parse textParser inputFilePath contents

-- | Genera un fichero PDF a partir de un PDFValue.
generatePdf :: PDFValue -> FilePath -> IO ()
generatePdf (PDFDocument contents) outputPath = do
  let -- 1. Procesar contenido y definir helpers
      initialFontState = FontState {fontFamily = "Helvetica", fontSize = 12}
      initialFoldState = ([], initialFontState)
      (pdfTextCommands, _finalState) = foldl processContent initialFoldState contents
      pad10 n = let s = show n in replicate (10 - length s) '0' ++ s
      -- 2. Construir objetos PDF como ByteStrings
      initialFontCmd = C8.pack $ fontRef (fontFamily initialFontState) ++ " " ++ show (fontSize initialFontState) ++ " Tf"
      contentStream = B.intercalate (C8.pack "\n") ([C8.pack "BT", initialFontCmd, C8.pack "12 TL", C8.pack "72 800 Td"] ++ pdfTextCommands ++ [C8.pack "ET"])
      contentLength = B.length contentStream      
      -- Genera la sección de recursos de fuente a partir del `fontRefMap`.
      -- Esto evita duplicar los nombres de las fuentes y sus referencias.
      fontResources = B.intercalate (C8.pack " ") $ map buildFontResource (toList fontRefMap)
      pdfObjects =
        [ C8.pack "%PDF-1.4",
          C8.pack "1 0 obj << /Type /Catalog /Pages 2 0 R >> endobj",
          C8.pack "2 0 obj << /Type /Pages /Kids [3 0 R] /Count 1 >> endobj",
          C8.pack "3 0 obj << /Type /Page /Parent 2 0 R /MediaBox [0 0 595 842] /Contents 4 0 R /Resources << /Font << "
            <> fontResources
            <> C8.pack " >> >> >> endobj",
          C8.pack ("4 0 obj << /Length " ++ show contentLength ++ " >> stream"),
          contentStream,
          C8.pack "endstream endobj"
        ]

      -- 3. Calcular offsets y construir la tabla xref y el trailer
      objectOffsets = scanl (\acc obj -> acc + fromIntegral (B.length obj) + 1) 0 pdfObjects
      xrefEntries =
        [ C8.pack "xref",
          C8.pack "0 5",
          C8.pack "0000000000 65535 f "
        ]
          ++ map (C8.pack . (++ " 00000 n ") . pad10) (init objectOffsets)
      trailerOffset = last objectOffsets
      trailer =
        [ C8.pack "trailer << /Root 1 0 R /Size 5 >>",
          C8.pack "startxref",
          C8.pack (show trailerOffset),
          C8.pack "%%EOF"
        ]

      -- 4. Unir todas las partes

      pdfContent = C8.unlines (pdfObjects ++ xrefEntries ++ trailer)

  withBinaryFile outputPath WriteMode (`B.hPutStr` pdfContent)
  where
    -- Construye la definición de un recurso de fuente para el diccionario de recursos del PDF.
    buildFontResource :: (String, String) -> B.ByteString
    buildFontResource (fontName, fontId) =
      C8.pack $
        fontId ++ " << /Type /Font /Subtype /Type1 /BaseFont /" ++ fontName
          ++ " /Encoding /WinAnsiEncoding >>"

    -- Itera sobre el contenido y genera los comandos PDF apropiados.
    -- El 'acc' (acumulador) es una tupla de (lista de comandos, tamaño de fuente actual).
    processContent (commands, fontState) content =
      case content of       
        CommandItem (Command name params) ->
          case name of
            "font" -> handleFontCommand (commands, fontState) params
            _ -> (commands, fontState) -- Ignora comandos no reconocidos
        TextLine str -> (commands ++ [B.concat [C8.pack "(", encodeWinAnsi str, C8.pack ") Tj T*"]], fontState)

    -- Codifica un String a un ByteString usando una correspondencia simple a WinAnsiEncoding.
    -- Escapa los caracteres especiales de literales de cadena de PDF: '(', ')', '\'.
    encodeWinAnsi :: String -> B.ByteString
    encodeWinAnsi = B.pack . concatMap escapeAndEncode
      where
        escapeAndEncode c | c == '(' || c == ')' || c == '\\' = [92, fromIntegral (fromEnum c)] -- Escapa con '\' (ASCII 92)
                          | fromEnum c < 256 = [fromIntegral (fromEnum c) :: Word8]
                          | otherwise = [63] -- Reemplaza caracteres no soportados con '?' (ASCII 63)
    fontRefMap :: Map String String
    fontRefMap = fromList [("Helvetica", "/F1"), ("Courier", "/F2"), ("Times-Roman", "/F3")]

    fontRef family = fontRefMap ! family -- Assumes family exists
    handleFontCommand (commands, initialFontState) params =
      let -- Procesa cada parámetro y actualiza el estado de la fuente.
          finalFontState = foldl updateFontState initialFontState params
          -- Genera el comando PDF si el estado de la fuente ha cambiado.
          (newCommands, newFontState)
            | finalFontState /= initialFontState =
                let newFontCmd = C8.pack $ fontRef (fontFamily finalFontState) ++ " " ++ show (fontSize finalFontState) ++ " Tf"
                    newLeadingCmd = C8.pack $ show (fontSize finalFontState) ++ " TL"
                 in (commands ++ [newFontCmd, newLeadingCmd], finalFontState)
            | otherwise = (commands, initialFontState)
       in (newCommands, newFontState)

    updateFontState state ("family", newFamily)
      | newFamily `elem` ["Helvetica", "Courier", "Times-Roman"] = state {fontFamily = newFamily}
      | otherwise = state -- Ignora familias de fuente no soportadas
    updateFontState state ("size", sizeStr) =
      case reads sizeStr of
        [(newSize, "")] -> state {fontSize = newSize}
        _ -> state -- Ignora tamaños no válidos
    updateFontState state _ = state -- Ignora parámetros no reconocidos

generatePdf _ _ = putStrLn "Tipo de PDFValue no soportado para la generación."
