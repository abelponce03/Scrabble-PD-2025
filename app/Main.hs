{-# LANGUAGE RecordWildCards #-}

module Main (main) where

import System.Random (randomRIO, newStdGen)
import Data.List (transpose, intersperse, find, sortBy, maximumBy, take)
import Data.Char (toUpper, toLower)
import Control.Monad (replicateM)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Data.Maybe (isJust)
import Lib

-- 1. Definir estructuras de datos
data Direction = Horizontal | Vertical deriving (Eq, Show)
data Player = Player { name :: String, score :: Int, tiles :: [Char] } deriving Show
data GameState = GameState {
    board :: [[Maybe Char]],
    players :: [Player],
    tileBag :: [Char],
    currentPlayer :: Int
} deriving Show

-- Tamaño del tablero
boardSize :: Int
boardSize = 15

-- Posición central del tablero
centerPosition :: (Int, Int)
centerPosition = (boardSize `div` 2, boardSize `div` 2)

-- Valores y distribución de fichas (simplificado)
tileDistribution :: [(Char, Int)]
tileDistribution = 
    [ ('A', 9), ('B', 2), ('C', 2), ('D', 4), ('E', 12)
    , ('F', 2), ('G', 3), ('H', 2), ('I', 9), ('J', 1)
    , ('L', 4), ('M', 2), ('N', 6), ('O', 8), ('P', 2)
    , ('Q', 1), ('R', 6), ('S', 4), ('T', 6), ('U', 4)
    , ('V', 2), ('Z', 1) ]

-- 2. Inicialización del juego
initializeGame :: IO GameState
initializeGame = do
    let bag = concatMap (\(c, n) -> replicate n c) tileDistribution
    shuffledBag <- shuffle bag
    let (p1Tiles, remaining1) = splitAt 7 shuffledBag
        (p2Tiles, remaining2) = splitAt 7 remaining1
        players = [ Player "Jugador 1" 0 p1Tiles
                  , Player "Jugador 2" 0 p2Tiles ]
        emptyBoard = replicate boardSize (replicate boardSize Nothing)
    return $ GameState emptyBoard players remaining2 0

-- Función para mezclar las fichas
shuffle :: [a] -> IO [a]
shuffle xs = do
    randomPositions <- mapM (\_ -> randomRIO (0, length xs - 1)) xs
    return $ map snd $ sortBy (comparing fst) $ zip randomPositions xs

-- 3. Mostrar el tablero
printBoard :: [[Maybe Char]] -> IO ()
printBoard board = do
    putStrLn "\nTablero:"
    mapM_ printRow board
    where
        printRow row = putStrLn $ intersperse ' ' $ map (maybe '.' id) row

-- 4. Lógica principal del juego
gameLoop :: GameState -> Trie -> Bool -> IO ()
gameLoop gameState@GameState{..} dictionary isFirstMove = do
    let current = players !! currentPlayer
    putStrLn $ "\nTurno de: " ++ name current
    printBoard board
    putStrLn $ "Tus fichas: " ++ tiles current
    
    putStrLn "Ingresa tu movimiento (formato: x y dirección [H|V] palabra) o 'pasar'"
    input <- getLine
    
    if input == "pasar"
        then advanceTurn gameState dictionary isFirstMove
        else case parseInput input of
            Just (x, y, dir, word) -> handleMove x y dir (map toUpper word) gameState dictionary isFirstMove
            Nothing -> do
                putStrLn "Formato inválido"
                gameLoop gameState dictionary isFirstMove

-- Definición de Trie
data Trie = Trie { endOfWord :: Bool, children :: [(Char, Trie)] } deriving (Show)

-- Crear un Trie vacío
emptyTrie :: Trie
emptyTrie = Trie False []

-- Insertar una palabra en el Trie
insertTrie :: String -> Trie -> Trie
insertTrie [] (Trie _ children) = Trie True children
insertTrie (c:cs) (Trie end children) =
    Trie end (insertChild c cs children)
  where
    insertChild :: Char -> String -> [(Char, Trie)] -> [(Char, Trie)]
    insertChild c cs [] = [(c, insertTrie cs emptyTrie)]
    insertChild c cs ((x, t):xs)
        | c == x    = (x, insertTrie cs t) : xs
        | otherwise = (x, t) : insertChild c cs xs

-- Buscar una palabra en el Trie
searchTrie :: String -> Trie -> Bool
searchTrie [] (Trie end _) = end
searchTrie (c:cs) (Trie _ children) = case lookup c children of
    Just child -> searchTrie cs child
    Nothing -> False

-- Leer palabras del archivo y construir el diccionario Trie
loadDictionary :: FilePath -> IO Trie
loadDictionary path = do
    content <- readFile path
    let wordsList = lines content
    return $ foldr (insertTrie . map toUpper) emptyTrie wordsList

-- Verificar si una jugada es válida (Optimizada)
isValidMove :: Int -> Int -> Direction -> String -> [[Maybe Char]] -> Trie -> Bool -> Either String Bool
isValidMove x y dir word board dictionary isFirstMove
    | not inBounds = Left "La palabra no cabe en el tablero."
    | not noOverlap = Left "La palabra se superpone incorrectamente."
    | not wordExists = Left $ "La palabra '" ++ word ++ "' no existe en el diccionario."
    | not validStart = Left "La palabra no toca ninguna otra palabra."
    | otherwise = Right True
  where
    inBounds = case dir of
        Horizontal -> x >= 0 && x + length word <= boardSize && y >= 0 && y < boardSize
        Vertical   -> y >= 0 && y + length word <= boardSize && x >= 0 && x < boardSize

    noOverlap = all canPlace (zip positions word)

    positions = case dir of
        Horizontal -> [(x + i, y) | i <- [0..length word - 1]]
        Vertical   -> [(x, y + i) | i <- [0..length word - 1]]

    canPlace ((i, j), c) = case board !! j !! i of
        Nothing -> True
        Just existingChar -> existingChar == c

    wordExists = searchTrie (map toUpper word) dictionary

    validStart = if isFirstMove
                 then (centerPosition `elem` positions)
                 else any (\(i, j) -> any (\(di, dj) -> isJust (board !! (j + dj) !! (i + di))) [(0, 1), (1, 0), (0, -1), (-1, 0)]) positions

-- 5. Procesar movimientos
handleMove :: Int -> Int -> Direction -> String -> GameState -> Trie -> Bool -> IO ()
handleMove x y dir word gameState@GameState{..} dictionary isFirstMove = do
    let current = players !! currentPlayer
    if all (`elem` tiles current) word
        then case isValidMove x y dir word board dictionary isFirstMove of
            Right True -> do
                let newBoard = placeWord x y dir word board
                    newTiles = removeTiles word (tiles current)
                    newScore = score current + sum (map tileValue word)
                    updatedPlayer = current { score = newScore, tiles = newTiles }
                    newPlayers = updateList currentPlayer updatedPlayer players
                    newGameState = gameState { board = newBoard, players = newPlayers }
                advanceTurn newGameState dictionary False
            Left errorMsg -> do
                putStrLn $ "Movimiento inválido: " ++ errorMsg
                gameLoop gameState dictionary isFirstMove
        else do
            putStrLn "No tienes esas fichas"
            gameLoop gameState dictionary isFirstMove

-- Colocar palabra en el tablero (versión simplificada)
placeWord :: Int -> Int -> Direction -> String -> [[Maybe Char]] -> [[Maybe Char]]
placeWord x y dir word board
    | dir == Horizontal = 
        take y board ++
        [take x row ++ map Just word ++ drop (x + length word) row] ++
        drop (y + 1) board
    | otherwise = 
        transpose $ placeWord y x Horizontal word (transpose board)
    where row = board !! y

-- 6. Sistema de puntuación
tileValue :: Char -> Int
tileValue c = case c of
    'A' -> 1; 'B' -> 3; 'C' -> 3; 'D' -> 2; 'E' -> 1
    'F' -> 4; 'G' -> 2; 'H' -> 4; 'I' -> 1; 'J' -> 8
    'L' -> 1; 'M' -> 3; 'N' -> 1; 'O' -> 1; 'P' -> 3
    'Q' -> 5; 'R' -> 1; 'S' -> 1; 'T' -> 1; 'U' -> 1
    'V' -> 4; 'Z' -> 10; _ -> 0

-- Funciones auxiliares

-- Implementación de parseInput
parseInput :: String -> Maybe (Int, Int, Direction, String)
parseInput input = case words input of
    [xStr, yStr, dirStr, word] -> do
        x <- readMaybe xStr
        y <- readMaybe yStr
        dir <- case map toUpper dirStr of
                "H" -> Just Horizontal
                "V" -> Just Vertical
                _   -> Nothing
        Just (x, y, dir, map toUpper word)
    _ -> Nothing

-- Implementación de updateList
updateList :: Int -> a -> [a] -> [a]
updateList index newVal list
    | index < 0 || index >= length list = list
    | otherwise = let (xs, _:ys) = splitAt index list in xs ++ [newVal] ++ ys

-- Implementación de removeTiles
removeTiles :: String -> [Char] -> [Char]
removeTiles word tiles = foldl removeOne tiles word
  where
    removeOne :: [Char] -> Char -> [Char]
    removeOne [] _ = []
    removeOne (x:xs) c
        | x == c    = xs
        | otherwise = x : removeOne xs c
        
-- Función advanceTurn
advanceTurn :: GameState -> Trie -> Bool -> IO ()
advanceTurn gameState@GameState{..} dictionary isFirstMove = do
    if checkGameEnd gameState
        then do
            putStrLn "¡Juego terminado! Puntuación final:"
            mapM_ (\p -> putStrLn $ name p ++ ": " ++ show (score p)) players
            let winner = maximumBy (comparing score) players
            putStrLn $ "¡Ganador: " ++ name winner ++ "!"
        else do
            let current = players !! currentPlayer
                tilesNeeded = 7 - length (tiles current)
                (newTiles, remainingBag) = splitAt tilesNeeded tileBag
                updatedPlayer = current { tiles = tiles current ++ newTiles }
                newPlayers = updateList currentPlayer updatedPlayer players
                nextPlayer = (currentPlayer + 1) `mod` length newPlayers
                newState = gameState { players = newPlayers, tileBag = remainingBag, currentPlayer = nextPlayer }
            gameLoop newState dictionary False

-- 7. Finalizar juego
checkGameEnd :: GameState -> Bool
checkGameEnd GameState{..} = null tileBag && any (null . tiles) players

main :: IO ()
main = do
    dictionary <- loadDictionary "dic/es.txt"
    gameState <- initializeGame
    gameLoop gameState dictionary True