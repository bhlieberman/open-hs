{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Substances where

import qualified Code as C
import qualified Control.Exception as E
import Data.Aeson
import qualified Data.ByteString as BS hiding (concatMap, filter)
import qualified Data.List.NonEmpty as NE
import GHC.Generics (Generic)
import Network.HTTP.Simple

data Result = Result
  { codes :: [C.Code],
    substance_class :: String,
    names :: [Object],
    references :: [Object],
    definition_type :: String,
    moieties :: [Object],
    definition_level :: String,
    uuid :: String,
    version :: String,
    structure :: Object,
    unii :: String
  }
  deriving (Generic, Show)

instance FromJSON Substances.Result

data Substance = Substance
  { meta :: Object,
    results :: NE.NonEmpty Substances.Result
  }
  deriving (Generic, Show)

instance FromJSON Substance

getSubstancesJson :: IO (Response BS.ByteString)
getSubstancesJson = httpBS "https://api.fda.gov/other/substance.json?limit=1"

filterCasCodes :: Substance -> [C.Code]
filterCasCodes sub =
  let results_ :: NE.NonEmpty Substances.Result
      results_ = results sub
      codes_ :: [C.Code]
      codes_ = codes $ NE.head results_
      filt :: C.Code -> Bool
      filt c =
        let sys = C.code_system c
         in sys == "CAS"
   in filter filt codes_

getResults :: IO ()
getResults = do
  E.bracket resp onError $
    \body -> do
      let json_ = eitherDecodeStrict body :: Either String Substance
      case json_ of
        (Right j) -> print $ filterCasCodes j
        (Left err) -> putStrLn ("Error " <> err)
  where
    json = getSubstancesJson
    resp = getResponseBody <$> json
    onError = const $ getResponseStatusCode <$> json
