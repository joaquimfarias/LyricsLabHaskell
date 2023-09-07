module LyricsLibService where

import Control.Exception
import System.IO.Error

import Artistas.ArtistaService as AServ
import Objetos.Artista

buscarArtistaPorID:: String -> IO String
buscarArtistaPorID idAlvo = do
  artista <- AServ.getArtista idAlvo
  if nome artista == "NULL" then return "Artista nao encontrado."
  else return (show artista)

buscarArtistaPorNome:: String -> IO [String]
buscarArtistaPorNome nomeAlvo = do
  resultado <- AServ.filtrarArtistasPorNome nomeAlvo
  return (map (\artista -> (show artista)) resultado)

buscarBanda:: String -> IO String
buscarBanda nomeBanda = return ("")

buscarMusica:: String -> IO String
buscarMusica nomeMusica = return ("")

filtrarMusicasPorTrecho:: String -> IO [String]
filtrarMusicasPorTrecho trecho = return [("")]

filtrarBandasPorGenero:: String -> IO [String]
filtrarBandasPorGenero genero = return [("")]

filtrarMusicasPorInstrumento:: String -> IO [String]
filtrarMusicasPorInstrumento instrumento = return [("")]

filtrarMusicasPorRitmo:: String -> IO [String]
filtrarMusicasPorRitmo ritmo = return [("")]

filtrarArtistasPorFuncao:: String -> IO [String]
filtrarArtistasPorFuncao funcao = do
  resultado <- AServ.filtrarArtistasPorFuncao funcao
  return (map (\artista -> (show artista)) resultado)

filtrarBandasPorInstrumento:: String -> IO [String]
filtrarBandasPorInstrumento instrumento = return [("")]

topArtistas:: Int -> IO [String]
topArtistas xMelhores = return [("")]

topBandas:: Int -> IO [String]
topBandas xMelhores = return [("")]

topMusicas:: Int -> IO [String]
topMusicas xMelhores = return [("")]
