{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.IO (hFlush, stdout)
import System.Random (randomRIO, newStdGen)
import Data.List (transpose, intersperse, find, sortBy, maximumBy, foldl', nub)
import Data.Char (toUpper)
import Control.Monad (replicateM, unless)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Data.Maybe (isNothing, isJust, mapMaybe, fromMaybe, listToMaybe, catMaybes)
import Data.Map (Map, fromList, (!), member, findWithDefault)
import qualified Data.Map as Map
import Data.Trie (Trie, fromList, submap, lookup, empty)
import qualified Data.Trie as T
import Data.Array (Array, array, (!), bounds, indices, (//), inRange)
import Data.Function (on)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as MapS

-- 1. Estructuras de datos optimizadas
data Direction = Horizontal | Vertical deriving (Eq, Show, Ord)
data Player = Player { 
    name :: String,
    score :: Int,
    tiles :: Map Char Int,
    passLastTurn :: Bool  -- Cambio a Map para contar frecuencia O(1)
} deriving Show

data Casilla = Casilla {
    contenido :: Maybe Char,
    multiplicadorLetra :: Int,
    multiplicadorPalabra :: Int
} deriving (Show, Eq)

data GameState = GameState {
    board :: Array (Int, Int) Casilla,  -- Array para acceso O(1)
    players :: [Player],
    tileBag :: [Char],
    currentPlayer :: Int
} deriving Show

-- Tamaño del tablero
boardSize :: Int
boardSize = 15
centerPos :: (Int, Int)
centerPos = (7, 7)

-- Configuración de valores optimizada
tileValues :: Map Char Int
tileValues = Map.fromList [
    ('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1),
    ('F', 4), ('G', 2), ('H', 4), ('I', 1), ('J', 8),
    ('L', 1), ('M', 3), ('N', 1), ('O', 1), ('P', 3),
    ('Q', 10), ('R', 1), ('S', 1), ('T', 1), ('U', 1),
    ('V', 4), ('Z', 10)]

-- Valores y distribución de fichas (simplificado)
tileDistribution :: [(Char, Int)]
tileDistribution = 
    [ ('A', 9), ('B', 2), ('C', 2), ('D', 4), ('E', 12)
    , ('F', 2), ('G', 3), ('H', 2), ('I', 9), ('J', 1)
    , ('L', 4), ('M', 2), ('N', 6), ('O', 8), ('P', 2)
    , ('Q', 1), ('R', 6), ('S', 4), ('T', 6), ('U', 4)
    , ('V', 2), ('Z', 1) ]

-- Configuración de multiplicadores del tablero
initialMultipliers :: (Int, Int) -> (Int, Int)
initialMultipliers pos = case pos of
    (0,0)  -> (1,3)   -- Esquinas: triple palabra
    (0,7)  -> (1,3)
    (7,0)  -> (1,3)
    (14,0) -> (1,3)
    (0,14) -> (1,3)
    (7,14) -> (1,3)
    (14,7) -> (1,3)
    (14,14)-> (1,3)
    (7,7)  -> (1,2)   -- Centro: doble palabra
    (1,1)  -> (3,1)   -- Triple letra
    (5,5)  -> (3,1)
    (9,9)  -> (3,1)
    (13,13)-> (3,1)
    (1,13) -> (3,1)
    (5,9)  -> (3,1)
    (9,5)  -> (3,1)
    (13,1) -> (3,1)
    (2,2)  -> (2,1)   -- Doble letra
    (3,3)  -> (2,1)
    (11,11)-> (2,1)
    (12,12)-> (2,1)
    (2,12) -> (2,1)
    (3,11) -> (2,1)
    (11,3) -> (2,1)
    (12,2) -> (2,1)
    _      -> (1,1)

-- Función para mezclar una lista
shuffle :: [a] -> IO [a]
shuffle xs = do
    randomPositions <- mapM (\_ -> randomRIO (0, length xs - 1)) xs
    return $ map snd $ sortBy (comparing fst) $ zip randomPositions xs

-- 2. Inicialización del juego con multiplicadores
initializeGame :: IO GameState
initializeGame = do
    let boardDef = array ((0,0), (14,14)) 
                     [((i,j), Casilla Nothing (fst mult) (snd mult)) 
                      | i <- [0..14], j <- [0..14], let mult = initialMultipliers (i,j)]
        bag = concatMap (\(c, n) -> replicate n c) tileDistribution
    shuffledBag <- shuffle bag
    let (p1Tiles, remaining1) = splitAt 7 shuffledBag
        (p2Tiles, remaining2) = splitAt 7 remaining1
        initialTiles1 = Map.fromListWith (+) [(c, 1) | c <- p1Tiles]
        initialTiles2 = Map.fromListWith (+) [(c, 1) | c <- p2Tiles]
        players = 
            [ Player { name = "Bot1", score = 0, tiles = initialTiles1, passLastTurn = False }
            , Player { name = "Bot2", score = 0, tiles = initialTiles2, passLastTurn = False }
            ]
    return $ GameState boardDef players remaining2 0

--Visualización del tablero con multiplicadores
printBoard :: Array (Int, Int) Casilla -> IO ()
printBoard board = do
    -- Imprime la cabecera de columnas, cada una en un campo de 5 caracteres.
    putStrLn ("\n    " ++ concat [showCol i | i <- [0..14]])
    -- Imprime cada fila con su número (formateado a 2 caracteres) y las casillas
    mapM_ printRow [0..14]
  where
    -- Para números de columna: si es de un dígito se centra en un campo de 5 caracteres,
    -- si tiene dos dígitos se ajusta también.
    showCol :: Int -> String
    showCol n
      | n < 10    = "  " ++ show n ++ "  "
      | otherwise = " "  ++ show n ++ "  "
      
    printRow :: Int -> IO ()
    printRow y = do
        let row = [ board Data.Array.! (x, y) | x <- [0..14] ]
            rowStr = concatMap showCasilla row
            rowLabel = if y < 10 then " " ++ show y else show y
        putStrLn $ rowLabel ++ "  " ++ rowStr

    -- Formatea cada casilla para que ocupe 5 caracteres.
    -- Si hay letra se muestra centrada; si hay un multiplicador se imprime su código;
    -- de lo contrario, se muestra un punto.
    showCasilla :: Casilla -> String
    showCasilla Casilla{..}
      | isJust contenido = "  " ++ [fromMaybe ' ' contenido] ++ "  "
      | multiplicadorPalabra > 1 = " TW  "
      | multiplicadorLetra > 1   = if multiplicadorLetra == 3 then " TL  " else " DL  "
      | otherwise = "  .  "

--Validación mejorada de la primera jugada
isValidFirstMove :: String -> [(Int, Int)] -> Bool
isValidFirstMove word positions = 
    length word >= 2 && centerPos `elem` positions




-- Encuentra las palabras cruzadas y devuelve una lista de tríos: (posición, dirección perpendicular, palabra cruzada)
findCrossWords :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla -> [((Int, Int), Direction, String)]
findCrossWords start dir word board = catMaybes $ map checkPos (wordPositions start dir word board)
  where
    perp = perpDirection dir
    checkPos pos =
      let cross = extractCrossWord pos perp board
          trimmed = dropWhile (== ' ') cross
      in if length trimmed > 1 then Just (pos, perp, trimmed) else Nothing


-- 3. Búsqueda de mejores movimientos usando Trie
findBestMove :: GameState -> Trie Bool -> Maybe (String, (Int, Int), Direction, Int)
findBestMove gameState@GameState{..} dictTrie = 
    listToMaybe $ sortBy (flip compare `on` (\(_,_,_,s) -> s)) scoredMoves
  where
    current = players !! currentPlayer
    possibleWords = generateValidWords (Map.keys (tiles current)) dictTrie
    scoredMoves = mapMaybe (scoreMove gameState) possibleWords

    generateValidWords :: [Char] -> Trie Bool -> [String]
    generateValidWords availableLetters trie =
        let tileMap = MapS.fromListWith (+) [(c, 1) | c <- availableLetters]  -- Conteo correcto de letras
            allWords = map (C8.unpack . fst) $ T.toList trie  -- Convertir ByteString a String
        in filter (canBeFormed tileMap) allWords

    canBeFormed :: MapS.Map Char Int -> String -> Bool
    canBeFormed tm word =
        case foldl' checkAndUse (Just tm) word of
            Just _  -> True
            Nothing -> False
      where
        checkAndUse :: Maybe (MapS.Map Char Int) -> Char -> Maybe (MapS.Map Char Int)
        checkAndUse Nothing _ = Nothing
        checkAndUse (Just m) c =
            let (maybeVal, newMap) = Map.updateLookupWithKey
                                       (\_ v -> if v > 1 then Just (v - 1) else Nothing)
                                       c m
            in case maybeVal of
                 Just _  -> Just newMap  -- Se pudo consumir la ficha
                 Nothing -> Nothing       -- No se encontró la ficha o no hay suficientes

    scoreMove :: GameState -> String -> Maybe (String, (Int, Int), Direction, Int)
    scoreMove state word =
        case validPositions of
            [] -> Nothing
            _  -> Just $ maximumBy (comparing (\(_,_,_,s) -> s)) (map (evaluatePosition state word) validPositions)
        where
            validPositions = getValidPositions state word

-- 4. Evaluación de posiciones con heurísticas
evaluatePosition :: GameState -> String -> ((Int, Int), Direction) -> (String, (Int, Int), Direction, Int)
evaluatePosition GameState{ board = brd, .. } word (pos, dir) = 
    (word, pos, dir, baseScore * wordMult + crossWordsBonus + centerBonus)
  where
    (baseScore, wordMult) = calculateBaseScore brd pos dir word
    crossWordsBonus = sum $ map (\(start, d, w) -> calculateCrossWordScore start d w brd) (findCrossWords pos dir word brd)
    positions = wordPositions pos dir word brd
    centerBonus = if any (== centerPos) positions then 50 else 0

-- 5. Optimización de funciones críticas
wordPositions :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla -> [(Int, Int)]
wordPositions (x,y) dir word board = filter (`isValidIndex` board) positions
  where
    positions = take (length word) $ iterate step (x,y)
    step (a, b) = case dir of
                    Horizontal -> (a+1, b)
                    Vertical   -> (a, b+1)

calculateBaseScore :: Array (Int, Int) Casilla -> (Int, Int) -> Direction -> String -> (Int, Int)
calculateBaseScore brd start dir word = foldl' acc (0, 1) (zip word positions)
  where
    positions = wordPositions start dir word brd
    acc (total, wm) (chr, pos) =
        let Casilla { multiplicadorLetra = lm, multiplicadorPalabra = wm' } = brd Data.Array.! pos
        in (total + (Map.findWithDefault 0 chr tileValues * lm), wm * wm')

usedLetters :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla -> [Char]
usedLetters pos dir word board =
    [ c 
    | (c, p) <- zip word (wordPositions pos dir word board)
    , isNothing (contenido (board Data.Array.! p))
    ]

-- 1. Lógica principal del juego
gameLoop :: GameState -> Trie Bool -> IO ()
gameLoop gameState@GameState{..} dictionary
    | checkGameEnd gameState = do
        putStrLn "¡Juego terminado! Puntuación final:"
        mapM_ (\p -> putStrLn $ name p ++ ": " ++ show (score p)) players
        let winner = maximumBy (comparing score) players
        putStrLn $ "¡Ganador: " ++ name winner ++ "!"
    | otherwise = do
        let current@Player{..} = players !! currentPlayer
        printBoard board
        putStrLn $ "\nTurno de: " ++ name
        putStrLn $ "Fichas disponibles: " ++ show (Map.toList tiles)
        
        if take 3 name == "Bot"
            then do
                case findBestMove gameState dictionary of
                    Just (word, pos, dir, _) -> do
                        putStrLn $ "Bot juega: " ++ word ++ " en " ++ show pos ++ " " ++ show dir
                        handleMove pos dir word gameState dictionary
                    
                    Nothing -> do
                        putStrLn "Bot pasa su turno"
                        let updatedPlayers = updateList currentPlayer (current { passLastTurn = True }) players
                            newState = gameState { players = updatedPlayers }
                        advanceTurn newState dictionary
            else do
                putStrLn "Ingresa tu movimiento (formato: x y H|V palabra) o 'pasar':"
                hFlush stdout
                input <- getLine
                
                if input == "pasar"
                    then do
                        let updatedPlayers = updateList currentPlayer (current { passLastTurn = True }) players
                            newState = gameState { players = updatedPlayers }
                        advanceTurn newState dictionary
                    else case parseInput input of
                        Just ((x,y), dir, word) -> 
                            handleMove (x,y) dir (map toUpper word) gameState dictionary
                        Nothing -> do
                            putStrLn "Formato inválido"
                            gameLoop gameState dictionary

-- 2. Manejo de movimientos con actualización de multiplicadores
handleMove :: (Int, Int) -> Direction -> String -> GameState -> Trie Bool -> IO ()
handleMove pos dir word gameState@GameState{..} dictionary = do
    let current = players !! currentPlayer
    if not (hasTiles current word)
        then do
            putStrLn "No tienes las fichas necesarias"
            gameLoop gameState dictionary
        else if not (isValidMove pos dir word gameState)
            then do
                putStrLn "Movimiento inválido"
                gameLoop gameState dictionary
            else do
                let (newBoard, usedMultipliers) = placeWord pos dir word board
                    crossWords = findCrossWords pos dir word newBoard
                    mainWordScore = calculateWordScore pos dir word newBoard
                    crossScores = sum [calculateWordScore start d w newBoard | (start,d,w) <- crossWords]
                    totalScore = mainWordScore + crossScores
                    updatedPlayers = updatePlayerScore currentPlayer totalScore players
                    newTileBag = tileBag ++ usedLetters pos dir word board
                    newState = gameState {
                        board = newBoard,
                        players = replenishTiles currentPlayer word updatedPlayers newTileBag,
                        tileBag = drop (length word) newTileBag
                    }
                putStrLn $ "Puntuación obtenida: " ++ show totalScore
                -- gameLoop newState dictionary
                advanceTurn newState dictionary
  where
    hasTiles player word = 
        all (\c -> Map.findWithDefault 0 c (tiles player) >= length (filter (==c) word)) 
            (nub word)

-- 3. Cálculo de puntuación con multiplicadores
calculateWordScore :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla -> Int
calculateWordScore (x,y) dir word board = 
    let positions = wordPositions (x,y) dir word board
        letters = zip word positions
        (letterScores, wordMultipliers) = foldl' processLetter (0, 1) letters
    in letterScores * wordMultipliers
  where
    processLetter (total, wm) (c, pos) =
        let Casilla _ lm wm' = board Data.Array.! pos
            letterValue = Map.findWithDefault 0 c tileValues * lm
        in (total + letterValue, wm * wm')

-- 4. Reposición de fichas
replenishTiles :: Int -> String -> [Player] -> [Char] -> [Player]
replenishTiles playerIndex word players newTiles =
    let current = players !! playerIndex
        remainingTiles = foldl' (\m c -> Map.update (\v -> if v > 1 then Just (v-1) else Nothing) c m) (tiles current) word
        tilesNeeded = 7 - Map.foldr (+) 0 remainingTiles
        (newLetters, remainingBag) = splitAt tilesNeeded newTiles
        updatedTiles = Map.unionWith (+) remainingTiles (Map.fromListWith (+) [(c,1) | c <- newLetters])
    in updateList playerIndex (current { tiles = updatedTiles }) players

-- 5. Validación completa de palabras cruzadas
validateCrossWords :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla -> Trie Bool -> Bool
validateCrossWords start dir word board trie =
    all isValidCrossWord (findCrossWords start dir word board)
  where
    isValidCrossWord (crossStart, crossDir, crossWord) =
      case crossDir of
        Horizontal -> T.member (C8.pack crossWord) trie
        Vertical   -> T.member (C8.pack crossWord) trie

-- Función auxiliar para actualizar multiplicadores
placeWord :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla 
          -> (Array (Int, Int) Casilla, [Char])
placeWord (x,y) dir word board = 
    (board Data.Array.// updates, map fst letters)
  where
    positions = wordPositions (x,y) dir word board
    letters = zip word positions
    updates = [ (pos, Casilla (Just c) 1 1) | (c, pos) <- letters ]

findCrossWord :: Array (Int, Int) Casilla -> (Int, Int) -> [((Int, Int), Direction, String)]
findCrossWord board (x,y) =
    [ (verticalStart, Vertical, verticalWord) | not (null verticalWord) ] ++
    [ (horizontalStart, Horizontal, horizontalWord) | not (null horizontalWord) ]
  where
    verticalWord = getVerticalWord board (x,y)
    horizontalWord = getHorizontalWord board (x,y)
    verticalStart = (x, findStart Vertical (x,y) board)
    horizontalStart = (findStart Horizontal (x,y) board, y)

getVerticalWord :: Array (Int, Int) Casilla -> (Int, Int) -> String
getVerticalWord board (x,y) =
    reverse (getLettersUp board (x,y)) ++ getLettersDown board (x,y)

getHorizontalWord :: Array (Int, Int) Casilla -> (Int, Int) -> String
getHorizontalWord board (x,y) =
    reverse (getLettersLeft board (x,y)) ++ getLettersRight board (x,y)

findStart :: Direction -> (Int, Int) -> Array (Int, Int) Casilla -> Int
findStart dir (x,y) board = case dir of
    Vertical -> last [yy | yy <- [0..y], isJust (contenido (board Data.Array.! (x,yy)))]
    Horizontal -> last [xx | xx <- [0..x], isJust (contenido (board Data.Array.! (xx,y)))]

-- Funciones auxiliares para obtener letras en todas direcciones
getLettersUp, getLettersDown, getLettersLeft, getLettersRight 
    :: Array (Int, Int) Casilla -> (Int, Int) -> String
getLettersUp board (x,y) = [ c | yy <- [y-1, y-2 .. 0], let c = getLetter board (x,yy), c /= ' ' ]
getLettersDown board (x,y) = [ c | yy <- [y..14], let c = getLetter board (x,yy), c /= ' ' ]
getLettersLeft board (x,y) = [ c | xx <- [x-1, x-2 .. 0], let c = getLetter board (xx,y), c /= ' ' ]
getLettersRight board (x,y) = [ c | xx <- [x..14], let c = getLetter board (xx,y), c /= ' ' ]

-- Función parseInput que faltaba
parseInput :: String -> Maybe ((Int, Int), Direction, String)
parseInput input = case words input of
    [x, y, dir, word] -> do
        xNum <- readMaybe x
        yNum <- readMaybe y
        direction <- case dir of
            "H" -> Just Horizontal
            "V" -> Just Vertical
            _   -> Nothing
        return ((xNum, yNum), direction, word)
    _ -> Nothing

-- Función updateList
updateList :: Int -> a -> [a] -> [a]
updateList n newVal list = take n list ++ newVal : drop (n + 1) list

-- Función advanceTurn
advanceTurn :: GameState -> Trie Bool -> IO ()
advanceTurn gameState@GameState{..} dictionary = 
    gameLoop gameState{currentPlayer = (currentPlayer + 1) `mod` length players} dictionary

-- Función checkGameEnd
checkGameEnd :: GameState -> Bool
checkGameEnd GameState{..} = 
    (null tileBag && any (null . tiles) players) || foldr (&&) True (getPassLastTurn players)

getPassLastTurn :: [Player] -> [Bool]
getPassLastTurn players = [x | Player{ passLastTurn = x } <- players]



getValidPositions :: GameState -> String -> [((Int, Int), Direction)]
getValidPositions gameState@GameState{ board = brd } word =
    [ ((x,y), dir)
    | x <- [0 .. boardSize - 1]
    , y <- [0 .. boardSize - 1]
    , dir <- [Horizontal, Vertical]
    , fits (x,y) dir (length word)
    , all (isEmpty brd) (wordPositions (x,y) dir word brd)
    ]
  where
    fits (x,y) Horizontal l = x + l <= boardSize
    fits (x,y) Vertical   l = y + l <= boardSize

    isEmpty brd pos = isNothing (contenido (brd Data.Array.! pos))

isValidMove :: (Int, Int) -> Direction -> String -> GameState -> Bool
isValidMove pos dir word GameState{..} =
    let positions = wordPositions pos dir word board
    in all (\p -> contenido (board Data.Array.! p) == Nothing) positions

updatePlayerScore :: Int -> Int -> [Player] -> [Player]
updatePlayerScore idx points players =
  updateList idx updatedPlayer players
  where
    player = players !! idx
    updatedPlayer = player { score = score player + points
                           , passLastTurn = False  -- Restablece este flag al jugar
                           }

calculateCrossWordScore :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla -> Int
calculateCrossWordScore pos dir word board = calculateWordScore pos dir word board

getLetter :: Array (Int, Int) Casilla -> (Int, Int) -> Char
getLetter board pos = maybe ' ' id (contenido (board Data.Array.! pos))


main :: IO ()
main = do
    putStrLn "Cargando diccionario..."
    dictContent <- readFile "dic/diccionario_filtrado.txt"
    let dictionary = T.fromList [ (C8.pack (map toUpper w), True) 
                                  | w <- lines dictContent ]
    gameState <- initializeGame
    gameLoop gameState dictionary




-- Devuelve la dirección perpendicular a la indicada
perpDirection :: Direction -> Direction
perpDirection Horizontal = Vertical
perpDirection Vertical   = Horizontal

-- Dada una posición, una dirección y el tablero, extrae la palabra cruzada.
extractCrossWord :: (Int, Int) -> Direction -> Array (Int, Int) Casilla -> String
extractCrossWord pos dir board = reverse left ++ middle ++ right
  where
    left  = takeWhile (/= ' ') $ tail $ map letterAt $ iterate (moveNeg dir) pos
    middle = case if isValidIndex pos board then contenido (board Data.Array.! pos) else Nothing of
               Just c -> [c]
               Nothing -> " "
    right = takeWhile (/= ' ') $ map letterAt $ iterate (move dir) pos

    letterAt p = if isValidIndex p board 
                 then fromMaybe ' ' (contenido (board Data.Array.! p))
                 else ' '

    move Horizontal (x,y) = (x+1, y)
    move Vertical   (x,y) = (x, y+1)
    moveNeg Horizontal (x,y) = (x-1, y)
    moveNeg Vertical   (x,y) = (x, y-1)  -- Ajuste: para vertical se resta solo la coordenada y

isValidIndex :: (Int, Int) -> Array (Int, Int) a -> Bool
isValidIndex pos arr = inRange (bounds arr) pos


