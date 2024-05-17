{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
module Substances where
    import qualified Control.Exception as E
    import Data.Aeson
    import Data.ByteString
    import GHC.Generics ( Generic )
    import Network.HTTP.Simple

    import qualified Code as C

    data Result = Result {
        codes :: [C.Code],
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
    } deriving (Generic, Show)

    instance FromJSON Substances.Result

    data Substance = Substance {
        meta :: Object,
        results :: [Substances.Result]
    } deriving (Generic, Show)

    instance FromJSON Substance

    getSubstancesJson :: IO (Response ByteString) 
    getSubstancesJson = httpBS "https://api.fda.gov/other/substance.json?limit=1"

    getResults :: IO ()
    getResults = do
        E.bracket resp onError $
            \body -> do
                let json_ = eitherDecodeStrict body :: Either String Substance
                case json_ of
                    (Right j) -> print $ results j
                    (Left err) -> putStrLn ("Error " <> err)
        where
            json = getSubstancesJson
            resp = getResponseBody <$> json
            onError = const $ getResponseStatusCode <$> json