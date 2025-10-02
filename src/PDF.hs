module PDF ( PDFArray (..),
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
    pdfStringToByteString )
where 

import Data.Word (Word8)
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as C8

-- | Codifica un String a un ByteString usando una correspondencia simple a WinAnsiEncoding.
-- Escapa los caracteres especiales de literales de cadena de PDF: '(', ')', '\'.
encodeWinAnsi :: String -> B.ByteString
encodeWinAnsi = B.pack . concatMap escapeAndEncode
  where
    escapeAndEncode c
      | c `elem` "()\\" = [92, fromIntegral (fromEnum c)]
      | fromEnum c < 256 = [fromIntegral (fromEnum c) :: Word8]
      | otherwise = [63] -- Reemplaza caracteres no soportados con '?'

data PDFBoolean = PDFTrue | PDFFalse
  deriving (Show, Eq)

pdfBooleanToByteString :: PDFBoolean -> B.ByteString
pdfBooleanToByteString PDFTrue = C8.pack "true"
pdfBooleanToByteString PDFFalse = C8.pack "false"

data PDFNumeric = PDFInt Int | PDFFloat Float
  deriving (Show, Eq)

pdfNumericToByteString :: PDFNumeric -> B.ByteString
pdfNumericToByteString (PDFInt i) = C8.pack (show i)
pdfNumericToByteString (PDFFloat f) = C8.pack (show f)

data PDFString = PDFString String
  deriving (Show, Eq)

pdfStringToByteString :: PDFString -> B.ByteString
pdfStringToByteString (PDFString str) = B.concat [C8.pack "(", encodeWinAnsi str, C8.pack ")"]

data PDFName = PDFName String
  deriving (Show, Eq)

pdfNameToByteString :: PDFName -> B.ByteString
pdfNameToByteString (PDFName name) = C8.pack ('/' : name)

pdfIndirectObjectToByteString :: PDFIndirectObject -> B.ByteString
pdfIndirectObjectToByteString (PDFIndirectObject objNum genNum pdfType) =
  B.concat
    [ C8.pack (show objNum ++ " " ++ show genNum ++ " obj\n"),
      pdfTypeToByteString pdfType,
      C8.pack "\nendobj"
    ]

newtype PDFArray = PDFArray [PDFType]
  deriving (Show, Eq)

newtype PDFDictionary = PDFDictionary [(PDFName, PDFType)]
  deriving (Show, Eq)

data PDFStream = PDFStream PDFDictionary B.ByteString
  deriving (Show, Eq)

data PDFIndirectReference = PDFIndirectReference Int Int
  deriving (Show, Eq)

data PDFIndirectObject = PDFIndirectObject Int Int PDFType
  deriving (Show, Eq)

data PDFType
  = PDFBoolType PDFBoolean
  | PDFNumType PDFNumeric
  | PDFStrType PDFString
  | PDFNameType PDFName
  | PDFArrType PDFArray
  | PDFDictType PDFDictionary
  | PDFStreamType PDFStream
  | PDFRefType PDFIndirectReference 
  | PDFIndirectObjectType PDFIndirectObject
  | PDFNullType 
  deriving (Show, Eq)

pdfTypeToByteString :: PDFType -> B.ByteString
pdfTypeToByteString (PDFBoolType b) = pdfBooleanToByteString b
pdfTypeToByteString (PDFNumType n) = pdfNumericToByteString n
pdfTypeToByteString (PDFStrType s) = pdfStringToByteString s
pdfTypeToByteString (PDFNameType n) = pdfNameToByteString n
pdfTypeToByteString (PDFRefType (PDFIndirectReference objNum genNum)) = C8.pack $ show objNum ++ " " ++ show genNum ++ " R"
pdfTypeToByteString (PDFArrType (PDFArray arr)) = B.concat [C8.pack "[ ", B.intercalate (C8.pack " ") (map pdfTypeToByteString arr), C8.pack " ]"]
pdfTypeToByteString (PDFDictType (PDFDictionary pairs)) =
  let toPairByteString (key, val) = B.concat [pdfNameToByteString key, C8.pack " ", pdfTypeToByteString val]
      dictContent = B.intercalate (C8.pack " ") (map toPairByteString pairs)
   in B.concat [C8.pack "<< ", dictContent, C8.pack " >>"]
pdfTypeToByteString PDFNullType = C8.pack "null"
pdfTypeToByteString (PDFStreamType (PDFStream (PDFDictionary pairs) streamData)) =
  let streamLength = B.length streamData
      lengthEntry = (PDFName "Length", PDFNumType (PDFInt streamLength))
      updatedPairs = lengthEntry : filter (\(PDFName k, _) -> k /= "Length") pairs
      dictWithLength = PDFDictType (PDFDictionary updatedPairs)
   in B.concat [pdfTypeToByteString dictWithLength, C8.pack "\nstream\n", streamData, C8.pack "\nendstream"]
pdfTypeToByteString (PDFIndirectObjectType (PDFIndirectObject objNum genNum pdfType)) =
  pdfIndirectObjectToByteString (PDFIndirectObject objNum genNum pdfType)