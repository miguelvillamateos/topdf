module Lib
  ( parseInput,
    generatePdf,
  )
where

import qualified Data.ByteString as B
import Data.Map (Map, fromList, toList, (!))
import PDF
  ( PDFArray (..),
    PDFDictionary (..),
    PDFIndirectObject (..),
    PDFIndirectReference (..),
    PDFName (..),
    PDFNumeric (..),
    PDFStream (..),  
    PDFString (..),
    PDFType (..),
    pdfIndirectObjectToByteString,  
    pdfTypeToByteString,
    pdfStringToByteString   
  )

import System.IO
  ( IOMode (ReadMode, WriteMode),
    hGetContents,
    hSetEncoding,
    utf8,
    withBinaryFile,
    withFile,
  )
import Text.Parsec (ParseError, alphaNum, char,option, endOfLine, eof, letter, many, many1, oneOf,noneOf, parse, sepBy, spaces, try, (<|>))
import qualified Data.ByteString.Char8 as C8
import Text.ParserCombinators.Parsec (GenParser)

data FontState = FontState
  { 
    charSpace :: Float,
    wordSpace :: Float,
    scale:: Double,
    leading :: Int,
    fontFamily :: String,
    fontSize :: Float
  }
  deriving (Eq)


type Param = (String, String)

data Command = Command String [Param]
  deriving (Show, Eq)

data Content
  = TextLine String
  | CommandItem Command
  deriving (Show, Eq)

data PDFValue = PDFDocument [Content]
  deriving (Show, Eq)

identifier :: GenParser Char st String
identifier = many1 letter

paramValue :: GenParser Char st String
paramValue = try floatStr <|> many1 alphaNum
  where
    floatStr = do
      integerPart <- many1 (oneOf "0123456789")
      decimalPart <- option "" $ do
        _ <- char '.'
        many1 (oneOf "0123456789")
      return (integerPart ++ if null decimalPart then "" else "." ++ decimalPart)

paramParser :: GenParser Char st Param
paramParser = do
  name <- identifier
  _ <- spaces *> char '=' <* spaces
  value <- paramValue
  return (name, value)

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
  contents <- (commandParser <|> textLine) `sepBy` endOfLine
  eof
  return $ PDFDocument contents

-- | Lee un fichero, lo parsea y devuelve el resultado o un error.
parseInput :: FilePath -> IO (Either ParseError PDFValue)
parseInput inputFilePath = withFile inputFilePath ReadMode $ \handle -> do
  hSetEncoding handle utf8
  contents <- hGetContents handle
  return $! parse textParser inputFilePath contents

-- | Función de ayuda para construir un comando de stream a partir de operandos y un operador.
buildStreamCommand :: [PDFType] -> String -> B.ByteString
buildStreamCommand operands operator =
  B.intercalate (C8.pack " ") (map pdfTypeToByteString operands ++ [C8.pack operator])

-- Construye la definición de un recurso de fuente para el diccionario de recursos del PDF.
buildFontResource :: (String, String) -> (PDFName, PDFType)
buildFontResource (fontName, fontId) =
  ( PDFName (drop 1 fontId),
    PDFDictType (PDFDictionary [(PDFName "Type", PDFNameType (PDFName "Font")), (PDFName "Subtype", PDFNameType (PDFName "Type1")), (PDFName "BaseFont", PDFNameType (PDFName fontName)), (PDFName "Encoding", PDFNameType (PDFName "WinAnsiEncoding"))])
  )

-- | Genera un fichero PDF a partir de un PDFValue.
generatePdf :: PDFValue -> FilePath -> IO ()
generatePdf (PDFDocument contents) outputPath = do
  let -- 1. Procesar contenido y definir helpers
      initialFontState = FontState {charSpace = 0, wordSpace = 0, scale = 4, leading = 0,fontFamily = "Helvetica", fontSize = 12}
      initialFoldState = ([], initialFontState)
      (pdfTextCommands, _finalState) = foldl processContent initialFoldState contents

      -- Rellena un número con ceros a la izquierda hasta tener 10 dígitos.
      pad10 n = let s = show n in replicate (10 - length s) '0' ++ s

      -- 2. Construir objetos PDF como ByteStrings
      initialFontCmd = buildStreamCommand [PDFNameType (PDFName (fontRef (fontFamily initialFontState))), PDFNumType (PDFFloat (fontSize initialFontState))] "Tf"
      initialLeadingCmd = buildStreamCommand [PDFNumType (PDFFloat (fontSize initialFontState))] "TL"
      initialTextPosCmd = buildStreamCommand [PDFNumType (PDFInt 72), PDFNumType (PDFInt 800)] "Td"

      contentStream = B.intercalate (C8.pack "\n") ([C8.pack "BT", initialFontCmd, initialLeadingCmd, initialTextPosCmd] ++ pdfTextCommands ++ [C8.pack "ET"])
      fontResourceDict = PDFDictType (PDFDictionary (map buildFontResource (toList fontRefMap)))
      -- Definición de los objetos PDF usando los tipos de datos.
      catalogObject =
        PDFIndirectObject 1 0 $
          PDFDictType $
            PDFDictionary
              [ (PDFName "Type", PDFNameType (PDFName "Catalog")),
                (PDFName "Pages", PDFRefType (PDFIndirectReference 2 0))
              ]

      pagesObject =
        PDFIndirectObject 2 0 $
          PDFDictType $
            PDFDictionary
              [ (PDFName "Type", PDFNameType (PDFName "Pages")),
                (PDFName "Kids", PDFArrType (PDFArray [PDFRefType (PDFIndirectReference 3 0)])),
                (PDFName "Count", PDFNumType (PDFInt 1))
              ]

      pageObject =
        PDFIndirectObject 3 0 $
          PDFDictType $
            PDFDictionary
              [ (PDFName "Type", PDFNameType (PDFName "Page")),
                (PDFName "Parent", PDFRefType (PDFIndirectReference 2 0)),
                (PDFName "MediaBox", PDFArrType (PDFArray [PDFNumType (PDFInt 0), PDFNumType (PDFInt 0), PDFNumType (PDFInt 595), PDFNumType (PDFInt 842)])),
                (PDFName "Contents", PDFRefType (PDFIndirectReference 4 0)),
                (PDFName "Resources", PDFDictType (PDFDictionary [(PDFName "Font", fontResourceDict)]))
              ]

      contentObject = PDFIndirectObject 4 0 $ PDFStreamType (PDFStream (PDFDictionary []) contentStream)

      encodingObject =
        PDFIndirectObject 5 0 $
          PDFDictType $
            PDFDictionary
              [ (PDFName "Type", PDFNameType (PDFName "Encoding")),
                (PDFName "BaseEncoding", PDFNameType (PDFName "WinAnsiEncoding"))
              ]

      -- Convierte los objetos a ByteString para el fichero.
      pdfFileBody =
        [ pdfIndirectObjectToByteString catalogObject,
          pdfIndirectObjectToByteString pagesObject,
          pdfIndirectObjectToByteString pageObject,
          pdfIndirectObjectToByteString contentObject,
          pdfIndirectObjectToByteString encodingObject
        ]

      pdfFileHeader = C8.pack "%PDF-1.4"

      -- 3. Calcular offsets y construir la tabla xref y el pdfFileTrailer
      -- El offset inicial tiene en cuenta la cabecera del PDF y el salto de línea.
      objectOffsets = scanl (\acc obj -> acc + (B.length obj) + 1) (B.length pdfFileHeader + 1) pdfFileBody
      xrefObjectCount = length pdfFileBody + 1 -- +1 para el objeto 0
      pdfCrossRefTable =
        [ C8.pack "xref",
          C8.pack ("0 " ++ show xrefObjectCount),
          C8.pack "0000000000 65535 f "
        ]
          ++ map (C8.pack . (++ " 00000 n ") . pad10) (init objectOffsets)

      trailerDict =
        PDFDictType $
          PDFDictionary
            [ (PDFName "Root", PDFRefType (PDFIndirectReference 1 0)),
              (PDFName "Size", PDFNumType (PDFInt xrefObjectCount))
            ]
      trailerOffset = last objectOffsets

      -- 4. Unir todas las partes y escribir en el fichero.
      pdfContent =
        C8.unlines $
          [pdfFileHeader]
            ++ pdfFileBody
            ++ pdfCrossRefTable
            ++ [ C8.pack "trailer " <> pdfTypeToByteString trailerDict,
                 C8.pack "startxref",
                 C8.pack (show trailerOffset),
                 C8.pack "%%EOF"
               ]

  withBinaryFile outputPath WriteMode (`B.hPutStr` pdfContent)
  where
    processContent (commands, fontState) content =
      case content of
        CommandItem (Command name params) ->
          case name of
            "font" -> handleFontCommand (commands, fontState) params
            _ -> (commands, fontState) -- Ignora comandos no reconocidos
        TextLine str -> (commands ++ [B.concat [pdfStringToByteString $ PDFString str, C8.pack " Tj T*"]], fontState)

    -- Mapa de familias de fuentes a sus identificadores de recursos en el PDF.
    fontRefMap :: Map String String
    fontRefMap = fromList [("Helvetica", "/F1"), ("Courier", "/F2"), ("Times-Roman", "/F3")]

    -- Busca el identificador de una fuente. Falla si la fuente no existe.
    fontRef family = fontRefMap ! family

    -- Maneja un comando \font, actualizando el estado de la fuente si es necesario.
    handleFontCommand (commands, initialFontState) params =
      let finalFontState = foldl updateFontState initialFontState params
          -- Genera nuevos comandos de stream solo si el estado de la fuente ha cambiado.
          generateCommands acc (key, changed) =
            if changed
              then case key of
                "font" -> buildStreamCommand [PDFNameType (PDFName (fontRef (fontFamily finalFontState))), PDFNumType (PDFFloat (fontSize finalFontState))] "Tf" : acc
                "leading" -> buildStreamCommand [PDFNumType (PDFFloat (fontSize finalFontState))] "TL" : acc
                "charSpace" -> buildStreamCommand [PDFNumType (PDFFloat (charSpace finalFontState))] "Tc" : acc
                "wordSpace" -> buildStreamCommand [PDFNumType (PDFFloat (wordSpace finalFontState))] "Tw" : acc
                _ -> acc
              else acc
          changedStates = [("font", fontFamily initialFontState /= fontFamily finalFontState || fontSize initialFontState /= fontSize finalFontState), ("leading", fontSize initialFontState /= fontSize finalFontState), ("charSpace", charSpace initialFontState /= charSpace finalFontState)]
          newCmds = foldl generateCommands [] changedStates
       in (commands ++ reverse newCmds, finalFontState)

    -- Actualiza el estado de la fuente basado en un solo parámetro.
    updateFontState state ("family", newFamily)
      | newFamily `elem` ["Helvetica", "Courier", "Times-Roman"] = state {fontFamily = newFamily}
      | otherwise = state -- Ignora familias de fuente no soportadas
    updateFontState state ("size", sizeStr) =
      case reads sizeStr of
        [(newSize, "")] -> state {fontSize = newSize}
        _ -> state -- Ignora tamaños no válidos
    updateFontState state ("charSpace", cSp) =
      case reads cSp of
        [(newCSp, "")] -> state {charSpace = newCSp :: Float}
        _ -> state -- Ignora tamaños no válidos
    updateFontState state ("wordSpace", wSp) =
      case reads wSp of
        [(newWSp, "")] -> state {wordSpace = newWSp :: Float}
        _ -> state -- Ignora tamaños no válidos
    updateFontState state _ = state -- Ignora parámetros no reconocidos
