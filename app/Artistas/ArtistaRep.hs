module Artistas.ArtistaRep where

import qualified Data.ByteString.Lazy as BS
import Objetos.Artista
import Data.Aeson

set:: [Artista] -> IO()
set artistas = do
  codificarArquivo artistas

codificarArquivo:: [Artista] -> IO()
codificarArquivo artistas = BS.writeFile "artista.json" (encode artistas)

get:: IO (Maybe [Artista])
get = do
  json <- BS.readFile "artista.json"
  return (decode json)