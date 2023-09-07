module Main (main) where

import qualified Data.ByteString.Lazy as BS
import Data.Aeson
import Lib

import LyricsLibService as LLS
import Objetos.Artista

main::IO()
main = do
  artista <- LLS.buscarArtistaPorNome "Bate"
  print artista

artistasList:: [Artista]
artistasList = [ 
  Artista "01" "Joseff" "Slipknada" ["Coldpause"] "Guitarrista",
  Artista "02" "Joseff2" "Slipknada2" ["Coldpause2"] "Guitarrista",
  Artista "03" "Joseff3" "Slipknada3" ["Coldpause3"] "Guitarrista",
  Artista "04" "Joseff4" "Slipknada4" ["Coldpause4"] "Guitarrista"
  ]
