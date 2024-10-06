module UplcBenchmark.ScriptSize (BinPath (..), SizeReport, mkSizeReport, sizeReportsToGnuPlotDat) where

import Data.Aeson (ToJSON (toJSON), object, (.=))
import Data.Kind (Type)
import Data.List (intercalate, sort)
import Data.String (IsString (fromString))
import System.Directory (listDirectory)
import System.FilePath (takeFileName, (</>))
import System.IO (IOMode (ReadMode), hFileSize, openFile)

type Sized :: Type
data Sized = Sized
  { sized'filePath :: FilePath
  , sized'size :: Integer
  }
  deriving stock (Show, Eq, Ord)

-- TODO: toEncoding

instance ToJSON Sized where
  toJSON sized =
    object
      [ "path" .= sized'filePath sized
      , "size" .= sized'size sized
      ]

mkSized :: FilePath -> IO Sized
mkSized filePath = do
  fileHandle <- openFile filePath ReadMode
  fileSize <- hFileSize fileHandle
  pure $ Sized (takeFileName filePath) fileSize

mkSizedDirectory :: FilePath -> IO [Sized]
mkSizedDirectory dirPath = do
  files <- listDirectory dirPath
  traverse (mkSized . (dirPath </>)) files

type BinPath :: Type
data BinPath = BinPath
  { binPath'name :: String
  , binPath'path :: FilePath
  }
  deriving stock (Show, Eq, Ord)

type SizeReport :: Type
data SizeReport = SizeReport
  { sizeReport'name :: String
  , sizeReport'files :: [Sized]
  }
  deriving stock (Show, Eq, Ord)

instance ToJSON SizeReport where
  toJSON sizeReport =
    object
      [ fromString (sizeReport'name sizeReport) .= sizeReport'files sizeReport
      ]

mkSizeReport :: BinPath -> IO SizeReport
mkSizeReport binPath = do
  files <- mkSizedDirectory $ binPath'path binPath
  pure $ SizeReport (binPath'name binPath) files

sizeReportsToGnuPlotDat :: [SizeReport] -> String
sizeReportsToGnuPlotDat reports = header (head reports) <> "\n" <> go reports
  where
    header :: SizeReport -> String
    header report =
      mconcat
        [ "Language,"
        , intercalate "," $ map sized'filePath $ sort $ sizeReport'files report
        ]

    go :: [SizeReport] -> String
    go [] = ""
    go (report : restOfReports) =
      mconcat
        [ sizeReport'name report
        , ","
        , intercalate "," $ map (show . sized'size) $ sort $ sizeReport'files report
        , "\n"
        , go restOfReports
        ]
