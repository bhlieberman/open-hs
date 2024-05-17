{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TemplateHaskell #-}
module Code where
    import Data.Aeson
    import Data.Aeson.TH
    import GHC.Generics ( Generic )


    data Code = Code {
        uuid :: String,
        code :: String,
        code_type :: String,
        url :: Maybe String,
        code_system :: String,
        references :: Maybe [String]
    } deriving (Generic, Show)

    $(deriveJSON defaultOptions {fieldLabelModifier = \x -> if x == "code_type" then "type" else x} ''Code)
