module Musicas.MusicaRep where

import qualified Data.ByteString.Lazy as BS
import Objetos.Musica
import Data.Aeson

setMusica :: [Musica] -> IO ()
setMusica musicas = do
  codificarArquivoMusica musicas

codificarArquivoMusica :: [Musica] -> IO ()
codificarArquivoMusica musicas = BS.writeFile "../musica.json" (encode musicas)

getMusica :: IO (Maybe [Musica])
getMusica = do
  json <- BS.readFile "../musica.json"
  return (decode json)