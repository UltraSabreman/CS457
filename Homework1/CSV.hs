{- |
   module: CSV 
   license: LGPL 
   maintainer: Jaap Weel <weel at ugcs dot caltech dot edu> 
   stability: provisional 
   portability: ghc 

   This module parses and dumps documents that are formatted more or
   less according to RFC 4180, \"Common Format and MIME Type for
   Comma-Separated Values (CSV) Files\",
   <http://www.rfc-editor.org/rfc/rfc4180.txt>.

   There are some issues with this RFC. I will describe what these
   issues are and how I deal with them.

   First, the RFC prescribes CRLF standard network line breaks, but
   you are likely to run across CSV files with other line endings, so
   we accept any sequence of CRs and LFs as a line break. 

   Second, there is an optional header line, but the format for the
   header line is exactly like a regular record and you can only
   figure out whether it exists from the mime type, which may not be
   available. I ignore the issues of header lines and simply turn them
   into regular records.
   
   Third, there is an inconsistency, in that the formal grammar
   specifies that fields can contain only certain US ASCII characters,
   but the specification of the MIME type allows for other character
   sets. I will allow all characters in fields, except for commas, CRs
   and LFs in unquoted fields. This should make it possible to parse
   CSV files in any encoding, but it allows for characters such as
   tabs that the RFC may be interpreted to forbid even in non-US-ASCII
   character sets. -}

{- Copyright (c) Jaap Weel 2007.  This library is free software; you
   can redistribute it and/or modify it under the terms of the GNU
   Lesser General Public License as published by the Free Software
   Foundation; either version 2.1 of the License, or (at your option)
   any later version.  This library is distributed in the hope that it
   will be useful, but WITHOUT ANY WARRANTY; without even the implied
   warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
   See the GNU Lesser General Public License for more details.  You
   should have received a copy of the GNU Lesser General Public
   License along with this library; if not, write to the Free Software
   Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA
   02110-1301 USA. -}
   
   
module CSV (CSV
                 , Record
                 , Field
                 , csv
                 , parseCSV
                 , parseCSVFromFile
                 , parseCSVTest
                 , printCSV
                 ) where

import Text.ParserCombinators.Parsec
import Data.List (intersperse)

-- | A CSV file is a series of records. According to the RFC, the
-- records all have to have the same length. As an extension, I
-- allow variable length records.
type CSV = [Record]

-- | A record is a series of fields
type Record = [Field]

-- | A field is a string
type Field = String

-- | A Parsec parser for parsing CSV files
csv :: Parser CSV
csv = do x <- record `sepEndBy` many1 (oneOf "\n\r")
         eof
         return x

record :: Parser Record
record = (quotedField <|> field) `sepBy` char ','

field :: Parser Field
field = many (noneOf ",\n\r\"")

quotedField :: Parser Field
quotedField = between (char '"') (char '"') $
              many (noneOf "\"" <|> try (string "\"\"" >> return '"'))

-- | Given a file name (used only for error messages) and a string to
-- parse, run the parser.
parseCSV :: FilePath -> String -> Either ParseError CSV
parseCSV = parse csv

-- | Given a file name, read from that file and run the parser
parseCSVFromFile :: FilePath -> IO (Either ParseError CSV)
parseCSVFromFile = parseFromFile csv

-- | Given a string, run the parser, and print the result on stdout.
parseCSVTest :: String -> IO ()
parseCSVTest = parseTest csv

-- | Given an object of type CSV, generate a CSV formatted
-- string. Always uses escaped fields.
printCSV :: CSV -> String
printCSV records = unlines (printRecord `map` records)
    where printRecord = concat . intersperse "," . map printField
          printField f = "\"" ++ concatMap escape f ++ "\""
          escape '"' = "\"\""
          escape x = [x]

