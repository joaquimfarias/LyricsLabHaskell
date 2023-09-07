module Artistas.ArtistaService where

import Artistas.ArtistaRep as ARep
import Objetos.Artista

{-Função Pública-}
setArtista:: Artista -> IO()
setArtista artista = do
  maybeArtistas <- ARep.get
  case maybeArtistas of
    Just artistasLista -> ARep.set (artista:artistasLista)
    Nothing -> return ()

{-Função Pública-}
getArtistas:: IO [Artista]
getArtistas = do
  maybeArtistas <- ARep.get
  case maybeArtistas of
    Just artistasLista -> return artistasLista
    Nothing -> return []

{-Função Pública-}
getArtista:: String -> IO Artista
getArtista idAlvo = do
  artistas <- getArtistas
  if null artistas then return (Artista "NULL" "NULL" "NULL" ["NULL"] "NULL")
  else return (findByID idAlvo artistas)

{-Função Pública-}
filtrarArtistasPorFuncao:: String -> IO [Artista]
filtrarArtistasPorFuncao funcao = do
  artistas <- getArtistas
  return (filtrByFunction funcao artistas [])

{-Função Pública-}
filtrarArtistasPorNome:: String -> IO [Artista]
filtrarArtistasPorNome nome = do
  artistas <- getArtistas
  return (filtrByName nome artistas [])

{-Função Pública-}
removerBandaAtual:: String -> IO()
removerBandaAtual idAlvo = do
  artistas <- getArtistas
  let novaLista = findAndRemoveBand idAlvo artistas []
  ARep.set novaLista
  return ()

{-Função Pública-}
alterarBandaAtual:: String -> String -> IO()
alterarBandaAtual idAlvo novaBanda = do
  artistas <- getArtistas
  let novaLista = findAndSwapBand idAlvo novaBanda artistas []
  ARep.set novaLista
  return ()

findByID:: String -> [Artista] -> Artista
findByID idAlvo (artista:resto)
  | null resto = do
    if idAlvo == (idArtista artista) then artista
    else Artista "NULL" "NULL" "NULL" ["NULL"] "NULL"
  | idAlvo == (idArtista artista) = artista
  | otherwise = findByID idAlvo resto

filtrByFunction:: String -> [Artista] -> [Artista] -> [Artista]
filtrByFunction funcaoAlvo (artista:resto) resultado
  | null resto = do
    if funcao artista == funcaoAlvo then (artista:resultado)
    else resultado
  | funcao artista == funcaoAlvo = filtrByFunction funcaoAlvo resto (artista:resultado)
  | otherwise = filtrByFunction funcaoAlvo resto resultado

filtrByName:: String -> [Artista] -> [Artista] -> [Artista]
filtrByName nomeAlvo (artista:resto) resultado
  | null resto = do
    if nome artista == nomeAlvo then (artista:resultado)
    else resultado
  | nome artista == nomeAlvo = filtrByName nomeAlvo resto (artista:resultado)
  | otherwise = filtrByName nomeAlvo resto resultado

findAndRemoveBand:: String -> [Artista] -> [Artista] -> [Artista]
findAndRemoveBand idAlvo (artista:resto) retorno
  | null resto = do
    if idArtista artista == idAlvo then do
      if (bandaAtual artista) == "Sem Banda" then (retorno++[artista])
      else if elem (bandaAtual artista) (bandasAnteriores artista) then do
        let novoArtista = Artista (idArtista artista) (nome artista) "Sem Banda" (bandasAnteriores artista) (funcao artista)
        (retorno++[novoArtista]++resto)
      else do
        let novoArtista = Artista (idArtista artista) (nome artista) "Sem Banda" (bandaAtual artista:(bandasAnteriores artista)) (funcao artista)
        (retorno++[novoArtista]++resto)
    else (retorno++[artista])
  | idArtista artista == idAlvo = do
    if (bandaAtual artista) == "Sem Banda" then (retorno++(artista:resto))
    else if elem (bandaAtual artista) (bandasAnteriores artista) then do
      let novoArtista = Artista (idArtista artista) (nome artista) "Sem Banda" (bandasAnteriores artista) (funcao artista)
      (retorno++[novoArtista]++resto)
    else do
      let novoArtista = Artista (idArtista artista) (nome artista) "Sem Banda" (bandaAtual artista:(bandasAnteriores artista)) (funcao artista)
      (retorno++[novoArtista]++resto)
  | otherwise = findAndRemoveBand idAlvo resto (retorno++[artista])

findAndSwapBand:: String -> String -> [Artista] -> [Artista] -> [Artista]
findAndSwapBand idAlvo novaBanda (artista:resto) retorno
  | null resto = do
    if idArtista artista == idAlvo then do
      if (bandaAtual artista) == novaBanda then (retorno++[artista])
      else if bandaAtual artista == "Sem Banda" then do
        let novoArtista = Artista (idArtista artista) (nome artista) novaBanda (bandasAnteriores artista) (funcao artista)
        (retorno++[novoArtista])
      else if elem (bandaAtual artista) (bandasAnteriores artista) then do
        let novoArtista = Artista (idArtista artista) (nome artista) novaBanda (bandasAnteriores artista) (funcao artista)
        (retorno++[novoArtista])
      else do
        let novoArtista = Artista (idArtista artista) (nome artista) novaBanda (bandaAtual artista:(bandasAnteriores artista)) (funcao artista)
        (retorno++[novoArtista])
    else (retorno++[artista])
  | idArtista artista == idAlvo = do
    if (bandaAtual artista) == novaBanda then (retorno++(artista:resto))
    else if bandaAtual artista == "Sem Banda" then do
        let novoArtista = Artista (idArtista artista) (nome artista) novaBanda (bandasAnteriores artista) (funcao artista)
        (retorno++[novoArtista]++resto)
    else if elem (bandaAtual artista) (bandasAnteriores artista) then do
      let novoArtista = Artista (idArtista artista) (nome artista) novaBanda (bandasAnteriores artista) (funcao artista)
      (retorno++[novoArtista]++resto)
    else do
      let novoArtista = Artista (idArtista artista) (nome artista) novaBanda (bandaAtual artista:(bandasAnteriores artista)) (funcao artista)
      (retorno++[novoArtista]++resto)
  | otherwise = findAndSwapBand idAlvo novaBanda resto (retorno++[artista])