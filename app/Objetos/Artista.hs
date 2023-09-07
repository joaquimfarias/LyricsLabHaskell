{-# LANGUAGE DeriveGeneric #-}
module Objetos.Artista where

  import Data.Aeson
  import GHC.Generics

  data Artista = Artista {
    idArtista:: String,
    nome:: String,
    bandaAtual:: String,
    bandasAnteriores:: [String],
    funcao:: String
  } deriving (Generic)

  instance ToJSON Artista
  instance FromJSON Artista
  instance Show Artista where
    show (Artista idArtista nome bandaAtual bandasAnteriores funcao) =
      "________________________________________\n" ++
      "Nome: " ++ nome ++ "\n" ++
      "Banda atual: " ++ bandaAtual ++ "\n" ++
      "Bandas anteriores\n" ++ bandasAnterioresToString bandasAnteriores "" ++ 
      "Função: " ++ funcao ++ "\n" ++
      "________________________________________\n"


  bandasAnterioresToString:: [String] -> String -> String
  bandasAnterioresToString [] resultado = "-- Nenhuma banda Anterior"
  bandasAnterioresToString (banda:resto) resultado
    | null resto = resultado ++ "- " ++ banda ++ "\n"
    | otherwise = bandasAnterioresToString resto (resultado ++ "-- " ++ banda ++ "\n")