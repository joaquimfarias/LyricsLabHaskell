{-# LANGUAGE DeriveGeneric #-}
module Objetos.Musica where

  import Data.Aeson
  import GHC.Generics

data Musica = Musica {
     idMusica :: String -- colocar como String
    , instrumentos :: [String]
    , participantes :: [String]
    , ritmo :: String
    , lancamento :: Day
    , letra :: String
    , nomeBanda :: String
    , avaliacao :: Int
    } deriving (Generic)


instance ToJSON Musica
instance FromJSON Musica
instance Show Musica where
    show (Musica idMusica instrumentos participantes ritmo lancamento letra nomeBanda avaliacao) =
        "________________________________________\n" ++
        "ID da Música: " ++ idMusica ++ "\n" ++
        "Instrumentos: " ++ show instrumentos ++ "\n" ++
        "Participantes: " ++ show participantes ++ "\n" ++
        "Ritmo: " ++ ritmo ++ "\n" ++
        "Data de Lançamento: " ++ show lancamento ++ "\n" ++
        "Letra:\n" ++ letra ++ "\n" ++
        "Nome da Banda: " ++ nomeBanda ++ "\n" ++
        "Avaliação: " ++ show avaliacao ++ "\n" ++
        "________________________________________"