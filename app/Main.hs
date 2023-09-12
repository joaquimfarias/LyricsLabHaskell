module Main (main) where

import Lib

import LyricsLibService as LLS
import Control.Exception
import Text.Read (readMaybe)
import Control.Concurrent (threadDelay)

main::IO()
main = do
  apresentacaoInicial
  resultado <- menuInicial
  if resultado == "0" then print ("Até mais!")
  else do
    case resultado of
      "11" -> do
        print ("###")
      "12" -> do
        print ("###")
      "13" -> do
        print ("###")
      "14" -> do
        print ("###")
      "15" -> do
        print ("###")
      "21" -> do
        putStrLn("Informe o nome do artista:")
        nome <- getLine
        busca <- LLS.buscarArtistaPorNome nome
        putStrLn (show busca)
      "22" -> do
        print ("###")
      "23" -> do
        print ("###")
      "24" -> do
        print ("###")
      "25" -> do
        print ("###")
      "26" -> do
        print ("###")
      "27" -> do
        print ("###")
      "28" -> do
        print ("###")
      "29" -> do
        print ("###")
      

apresentacaoInicial:: IO()
apresentacaoInicial = do
  print("Bem vindo ao Lyrics-LIB")
  threadDelay (2 * 1000000)
  putStrLn $ replicate 50 '\n'

menuInicial:: IO (String)
menuInicial = do
  putStrLn("----------")
  putStrLn("1 - Cadastros")
  putStrLn("2 - Buscas")
  putStrLn("3 - DashBoard")
  putStrLn("0 - Sair")
  putStrLn("----------\n\n")
  entrada <- getLine
  let entradaConvertida = readMaybe entrada :: Maybe Int
  case entradaConvertida of
    Just numero -> do
      case numero of
        1 -> do
          menuCadastros "1"
        2 -> do
          menuBuscas "2"
        3 -> do
          menuDashBoard
        0 -> return ("0")
    Nothing -> do
      putStrLn("\n\nOpção errada, por favor digite uma opcao valida.\n\n")
      menuInicial

menuCadastros:: String -> IO (String)
menuCadastros num1 = do
  putStrLn("----------")
  putStrLn("1 - Cadastrar Artista")
  putStrLn("2 - Cadastrar Banda")
  putStrLn("3 - Cadastrar Musica")
  putStrLn("----------")
  putStrLn("4 - Adicionar Integrantes na Banda")
  putStrLn("5 - Remover Integrantes na Banda")
  putStrLn("----------\n\n")
  return ("###")

menuBuscas:: String -> IO (String)
menuBuscas num1 = do
  putStrLn("----------")
  putStrLn("1 - Buscar Artista")
  putStrLn("2 - Buscar Banda")
  putStrLn("3 - Buscar Musica")
  putStrLn("----------")
  putStrLn("4 - Filtrar Artistas")
  putStrLn("5 - Filtrar Bandas")
  putStrLn("6 - Filtrar Musicas")
  putStrLn("----------")
  putStrLn("7 - Top Artistas")
  putStrLn("8 - Top Bandas")
  putStrLn("9 - Top Musicas")
  putStrLn("----------\n\n")
  entrada <- getLine
  let entradaConvertida = readMaybe entrada :: Maybe Int
  case entradaConvertida of
    Just numero -> do
      case numero of
        1 -> return (num1++"1")
        2 -> return (num1++"2")
        3 -> return (num1++"3")
        4 -> return (num1++"4")
        5 -> return (num1++"5")
        6 -> return (num1++"6")
        7 -> return (num1++"7")
        8 -> return (num1++"8")
        9 -> return (num1++"9")

    Nothing -> do
      putStrLn("\n\nOpção errada, por favor digite uma opcao valida.\n\n")
      menuBuscas num1


menuDashBoard:: IO (String)
menuDashBoard = return ("Em Breve!")