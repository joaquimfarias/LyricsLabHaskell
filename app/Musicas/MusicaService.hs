module Musicas.MusicaService where

import Musicas.MusicaRep as MRep
import Objetos.Musica
import Data.Time.Calendar

{-Função Pública-}
setMusica:: Musica -> IO()
setMusica musica = do
  maybeArtistas <- MRep.get
  case maybeArtistas of
    Just artistasLista -> MRep.set (artista:artistasLista)
    Nothing -> return ()

{-Função Pública-}
getMusicas :: IO [Musica]
getMusicas = do
  maybeMusicas <- getMusica
  case maybeMusicas of
    Just musicasLista -> return musicasLista
    Nothing -> return []

{-Função Pública-}
getMusicaPorID :: String -> IO Musica
getMusicaPorID idAlvo = do
  musicas <- getMusicas
  case findMusicaByID idAlvo musicas of
    Just musicaEncontrada -> return musicaEncontrada
    Nothing -> return (Musica "NULL" [] [] "NULL" (fromGregorian 0 0 0) "NULL" "NULL" 0)

findMusicaByID :: String -> [Musica] -> Maybe Musica
findMusicaByID _ [] = Nothing  -- Se a lista estiver vazia, retornar Nothing
findMusicaByID idAlvo (musica:resto)
  | idAlvo == idMusica musica = Just musica  -- Se o ID for encontrado, retornar Just com a música
  | otherwise = findMusicaByID idAlvo resto  -- Caso contrário, continuar a busca na lista restante



{-Função Pública-}
filtrarMusicasPorBanda :: String -> IO [Musica]
filtrarMusicasPorBanda nomeBanda = do
  musicas <- getMusicas
  return (filterByBanda nomeBanda musicas [])

filterByBanda :: String -> [Musica] -> [Musica] -> [Musica]
filterByBanda _ [] result = result
filterByBanda nomeBanda (musica:musicasRestantes) result
  | nomeBanda == nomeBanda (nomeBanda musica) = filterByBanda nomeBanda musicasRestantes (musica:result)
  | otherwise = filterByBanda nomeBanda musicasRestantes result

{-Função Pública-}
filtrarMusicasPorParticipante :: String -> IO [Musica]
filtrarMusicasPorParticipante nomeParticipante = do
  musicas <- getMusicas
  return (filter (\musica -> nomeParticipante `elem` participantes musica) musicas)

filterByParticipante :: String -> [Musica] -> [Musica] -> [Musica]
filterByParticipante _ [] result = result
filterByParticipante nomeParticipante (musica:musicasRestantes) result
  | nomeParticipante `elem` participantes musica = filterByParticipante nomeParticipante musicasRestantes (musica:result)
  | otherwise = filterByParticipante nomeParticipante musicasRestantes result

{-Função Pública-}
filtrarMusicasPorRitmo :: String -> IO [Musica]
filtrarMusicasPorRitmo nomeRitmo = do
  musicas <- getMusicas
  return (filterByRitmo nomeRitmo musicas [])

filterByRitmo :: String -> [Musica] -> [Musica] -> [Musica]
filterByRitmo _ [] result = result
filterByRitmo nomeRitmo (musica:musicasRestantes) result
  | nomeRitmo == ritmo musica = filterByRitmo nomeRitmo musicasRestantes (musica:result)
  | otherwise = filterByRitmo nomeRitmo musicasRestantes result

{-Função Pública-}
filtrarMusicasPorAno :: Integer -> [Musica] -> [Musica]
filtrarMusicasPorAno ano musicas =
  filter (\musica -> extractYear (lancamento musica) == ano) musicas
  where
    extractYear :: Day -> Integer
    extractYear day = toInteger $ (\(y, _, _) -> y) (toGregorian day)


{-Função Pública-}
filtrarMusicasPorTrechoLetra :: String -> [Musica] -> [Musica]
filtrarMusicasPorTrechoLetra trecho musicas =
  filter (\musica -> trecho `isInfixOf` letra musica) musicas
  where
    isInfixOf :: Eq a => [a] -> [a] -> Bool
    isInfixOf [] _ = True
    isInfixOf _ [] = False
    isInfixOf needle haystack@(x:xs) =
      if isPrefixOf needle haystack
        then True
        else isInfixOf needle xs
