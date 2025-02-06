{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import System.IO (hFlush, stdout)
import System.Random (randomRIO)
import Data.List (sortBy, maximumBy, foldl', nub)
import Data.Char (toUpper)
import Data.Ord (comparing)
import Text.Read (readMaybe)
import Data.Maybe (fromJust, isNothing, isJust, mapMaybe, fromMaybe, listToMaybe, catMaybes)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Trie (Trie)
import qualified Data.Trie as T
import Data.Array (Array, array, (!), bounds, (//), inRange)
import Data.Function (on)
import qualified Data.ByteString.Char8 as C8
import qualified Data.Map.Strict as MapS
import Control.Monad (forM_)
import System.Console.ANSI (clearScreen, setCursorPosition)

clearConsole :: IO ()
clearConsole = do
  clearScreen
  setCursorPosition 0 0

-- 1. Estructuras de datos optimizadas
-- Se define el tipo de datos para la dirección en la que se puede colocar una palabra en el tablero.
data Direction = Horizontal | Vertical deriving (Eq, Show, Ord)

-- Se define el tipo de datos para representar a un jugador con sus propiedades:
-- - name: el nombre del jugador.
-- - score: la puntuación acumulada.
-- - tiles: un mapa que asocia cada letra con la cantidad que el jugador posee.
-- - passLastTurn: indica si el jugador pasó en su último turno.
-- - moves: una lista de jugadas donde cada jugada es una tupla (palabra, puntos obtenidos).
data Player = Player { 
    name :: String,
    score :: Int,
    tiles :: Map Char Int, -- Mapa de fichas y su cantidad.
    passLastTurn :: Bool,
    moves :: [(String, Int)]  -- Lista de jugadas (palabra, puntos)
} deriving Show

-- Se define el tipo para las casillas del tablero:
-- - contenido: puede contener una letra (Just Char) o estar vacía (Nothing).
-- - multiplicadorLetra: valor multiplicador para fichas.
-- - multiplicadorPalabra: valor multiplicador para la palabra.
data Casilla = Casilla {
    contenido :: Maybe Char,
    multiplicadorLetra :: Int,
    multiplicadorPalabra :: Int
} deriving (Show, Eq)

-- Estado del juego que contiene:
-- - board: un arreglo bidimensional del tablero para acceder en tiempo O(1).
-- - players: la lista de jugadores.
-- - tileBag: la bolsa de fichas disponibles para el juego.
-- - currentPlayer: índice del jugador que tiene el turno actual.
data GameState = GameState {
    board :: Array (Int, Int) Casilla,  -- Usamos Array para acceso rápido.
    players :: [Player],
    tileBag :: [Char],
    currentPlayer :: Int
} deriving Show

-- Tamaño del tablero (15x15 en este caso)
boardSize :: Int
boardSize = 15

-- Posición central del tablero, importante para la jugada inicial.
centerPos :: (Int, Int)
centerPos = (7, 7)

-- Valores asignados a cada ficha (letra) según sus puntos de juego.
tileValues :: Map Char Int
tileValues = Map.fromList [
    ('A', 1), ('B', 3), ('C', 3), ('D', 2), ('E', 1),
    ('F', 4), ('G', 2), ('H', 4), ('I', 1), ('J', 8),
    ('L', 1), ('M', 3), ('N', 1), ('O', 1), ('P', 3),
    ('Q', 10), ('R', 1), ('S', 1), ('T', 1), ('U', 1),
    ('V', 4), ('Z', 10)]

-- Distribución de fichas: indica cuántas fichas hay de cada letra en la bolsa.
tileDistribution :: [(Char, Int)]
tileDistribution = 
    [ ('A', 9), ('B', 2), ('C', 2), ('D', 4), ('E', 12)
    , ('F', 2), ('G', 3), ('H', 2), ('I', 9), ('J', 1)
    , ('L', 4), ('M', 2), ('N', 6), ('O', 8), ('P', 2)
    , ('Q', 1), ('R', 6), ('S', 4), ('T', 6), ('U', 4)
    , ('V', 2), ('Z', 1) ]

-- Configuración de multiplicadores del tablero:
-- Esta función asigna, según la posición, multiplicadores a la casilla:
-- El primer valor es para la letra y el segundo para la palabra.
initialMultipliers :: (Int, Int) -> (Int, Int)
initialMultipliers pos = case pos of
    (0,0)  -> (1,3)   -- Esquinas: triple para palabra.
    (0,7)  -> (1,3)
    (7,0)  -> (1,3)
    (14,0) -> (1,3)
    (0,14) -> (1,3)
    (7,14) -> (1,3)
    (14,7) -> (1,3)
    (14,14)-> (1,3)
    (7,7)  -> (1,2)   -- Centro: doble para palabra.
    (1,1)  -> (3,1)   -- Posiciones con triple letra.
    (5,5)  -> (3,1)
    (9,9)  -> (3,1)
    (13,13)-> (3,1)
    (1,13) -> (3,1)
    (5,9)  -> (3,1)
    (9,5)  -> (3,1)
    (13,1) -> (3,1)
    (2,2)  -> (2,1)   -- Posiciones con doble letra.
    (3,3)  -> (2,1)
    (11,11)-> (2,1)
    (12,12)-> (2,1)
    (2,12) -> (2,1)
    (3,11) -> (2,1)
    (11,3) -> (2,1)
    (12,2) -> (2,1)
    _      -> (1,1)

-- Función para mezclar una lista de elementos de forma aleatoria.
-- Se asignan posiciones aleatorias a cada elemento y se reordena la lista con base en estos valores.
shuffle :: [a] -> IO [a]
shuffle xs = do
    -- Genera una lista de números aleatorios del mismo tamaño que xs.
    randomPositions <- mapM (\_ -> randomRIO (0, length xs - 1)) xs
    -- Empareja cada posición aleatoria con el elemento correspondiente,
    -- luego los ordena según el valor aleatorio y extrae el elemento.
    return $ map snd $ sortBy (comparing fst) $ zip randomPositions xs

-- 2. Inicialización del juego con multiplicadores
-- Inicialización del juego con multiplicadores.
-- Crea el estado inicial del tablero, la bolsa de fichas y asigna fichas a cada jugador.
initializeGame :: IO GameState
initializeGame = do
    -- Se define el tablero como un array, donde cada casilla se inicializa con un multiplicador según su posición.
    let boardDef = array ((0,0), (14,14)) 
                     [((i,j), Casilla Nothing (fst mult) (snd mult)) 
                      | i <- [0..14], j <- [0..14], let mult = initialMultipliers (i,j)]
        -- Se crea la bolsa de fichas de acuerdo a la distribución, replicando cada ficha según su cantidad.
        bag = concatMap (\(c, n) -> replicate n c) tileDistribution

    -- Se mezcla la bolsa de fichas.
    shuffledBag <- shuffle bag
    -- Se asignan las primeras 7 fichas a cada uno de los dos jugadores.
    let (p1Tiles, remaining1) = splitAt 7 shuffledBag
        (p2Tiles, remaining2) = splitAt 7 remaining1
        -- Se crea un mapa (contando ocurrencias) de fichas para cada jugador.
        initialTiles1 = Map.fromListWith (+) [(c, 1) | c <- p1Tiles]
        initialTiles2 = Map.fromListWith (+) [(c, 1) | c <- p2Tiles]
        -- Se definen dos jugadores con nombre, puntaje inicial, fichas asignadas, y otras propiedades.
        players = 
            [ Player { name = "Bot1", score = 0, tiles = initialTiles1, passLastTurn = False, moves = [] }
            , Player { name = "Bot4", score = 0, tiles = initialTiles2, passLastTurn = False, moves = [] }
            ]
    return $ GameState boardDef players remaining2 0


-- Encuentra las palabras cruzadas y devuelve una lista de tríos: (posición, dirección perpendicular, palabra cruzada)
-- Encuentra las palabras cruzadas en el tablero formadas al colocar una palabra.
-- Devuelve una lista de tríos que contienen la posición de inicio de la palabra cruzada,
-- la dirección perpendicular (vertical u horizontal) y la palabra encontrada.
findCrossWords :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla -> [((Int, Int), Direction, String)]
findCrossWords start dir word board =
    -- Se evalúa cada posición potencial donde la palabra puede generan cruces.
    catMaybes $ map checkPos (wordPositions start dir word board)
  where
    -- Se obtiene la dirección perpendicular (si la palabra es horizontal, la perpendicular es vertical y viceversa).
    perp = perpDirection dir
    checkPos pos =
      let cross = extractCrossWord pos perp board   -- Extrae la palabra en dirección perpendicular.
          trimmed = dropWhile (== ' ') cross         -- Quita espacios a la izquierda.
      in if length trimmed > 1 then Just (pos, perp, trimmed)  -- Registra la palabra si tiene longitud mayor a 1.
         else Nothing


-- 3. Búsqueda de mejores movimientos usando Trie
-- Búsqueda del mejor movimiento utilizando un Trie que contiene palabras válidas.
-- Se evalúan las jugadas según heurísticas y se retorna la de mayor puntaje.
findBestMove :: GameState -> Trie Bool -> Maybe (String, (Int, Int), Direction, Int)
findBestMove gameState@GameState{..} dictTrie = 
    listToMaybe $ sortBy (flip compare `on` (\(_,_,_,s) -> s)) scoredMoves
  where
    current = players !! currentPlayer
    -- Se generan las palabras posibles a formar con las fichas disponibles del jugador.
    possibleWords = generateValidWords (Map.keys (tiles current)) dictTrie
    -- Se evalúa la puntuación de cada movimiento y se filtran los movimientos válidos.
    scoredMoves = mapMaybe (scoreMove gameState) possibleWords

    -- Función interna para generar palabras válidas basadas en las fichas del jugador.
    generateValidWords :: [Char] -> Trie Bool -> [String]
    generateValidWords availableLetters trie =
        let tileMap = MapS.fromListWith (+) [(c, 1) | c <- availableLetters]  -- Se cuenta la cantidad de fichas disponibles.
            allWords = map (C8.unpack . fst) $ T.toList trie  -- Se convierten todas las palabras del Trie de ByteString a String.
        in filter (canBeFormed tileMap) allWords  -- Se filtran las palabras que se pueden formar.

    -- Verifica si una palabra se puede formar con el mapa de fichas disponibles.
    canBeFormed :: MapS.Map Char Int -> String -> Bool
    canBeFormed tm word =
        case foldl' checkAndUse (Just tm) word of
            Just _  -> True   -- Se pudo formar la palabra consumiendo las fichas.
            Nothing -> False  -- No es posible formar la palabra.
      where
        checkAndUse :: Maybe (MapS.Map Char Int) -> Char -> Maybe (MapS.Map Char Int)
        checkAndUse Nothing _ = Nothing
        checkAndUse (Just m) c =
            let (maybeVal, newMap) = Map.updateLookupWithKey
                                       (\_ v -> if v > 1 then Just (v - 1) else Nothing)
                                       c m
            in case maybeVal of
                 Just _  -> Just newMap  -- Se reduce el contador de la ficha usada.
                 Nothing -> Nothing       -- La ficha no era suficiente o no estaba presente

    -- Evalúa el movimiento para asignarle un puntaje en base a diversas heurísticas.
    scoreMove :: GameState -> String -> Maybe (String, (Int, Int), Direction, Int)
    scoreMove state word =
        case validPositions of
            [] -> Nothing   -- Si no existen posiciones válidas para colocar la palabra, se descarta.
            _  -> Just $ maximumBy (comparing (\(_,_,_,s) -> s)) (map (evaluatePosition state word) validPositions)
        where
            validPositions = getValidPositions state word

-- 4. Evaluación de posiciones con heurísticas
-- Evalúa una posición para colocar una palabra utilizando varias heurísticas:
-- puntaje base, bonificaciones por palabras cruzadas y un bono si se toca la casilla central.
evaluatePosition :: GameState -> String -> ((Int, Int), Direction) -> (String, (Int, Int), Direction, Int)
evaluatePosition GameState{ board = brd } word (pos, dir) = 
    (word, pos, dir, baseScore * wordMult + crossWordsBonus + centerBonus)
  where
    -- Calcula el puntaje base de la palabra basado en el tablero (fichas y multiplicadores).
    (baseScore, wordMult) = calculateBaseScore brd pos dir word
    -- Suma el puntaje adicional por cada palabra cruzada formada.
    crossWordsBonus = sum $ map (\(start, d, w) -> calculateCrossWordScore start d w brd) (findCrossWords pos dir word brd)
    -- Posiciones que la palabra ocupará.
    positions = wordPositions pos dir word brd
    -- Bono adicional si alguna posición es la central del tablero.
    centerBonus = if any (== centerPos) positions then 50 else 0

-- 5. Optimización de funciones críticas
-- Genera la lista de posiciones (índices) que la palabra ocupará en el tablero.
-- Sólo se retienen las posiciones que son índices válidos en el array del tablero.
wordPositions :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla -> [(Int, Int)]
wordPositions (x,y) dir word board = filter (`isValidIndex` board) positions
  where
    -- Crea una lista iterativa de posiciones comenzando en (x,y), moviéndose de acuerdo a la dirección.
    positions = take (length word) $ iterate step (x,y)
    step (a, b) =
        case dir of
            Horizontal -> (a+1, b)
            Vertical   -> (a, b+1)

-- Función calculateBaseScore calcula la puntuación base de una palabra en el tablero.
-- Recibe un array que representa el tablero, una posición inicial, una dirección y la palabra a colocar.
-- Devuelve una tupla (total, multiplicadorPalabra) que representa la puntuación base y el multiplicador acumulado.

calculateBaseScore :: Array (Int, Int) Casilla -> (Int, Int) -> Direction -> String -> (Int, Int)
calculateBaseScore brd start dir word = 
  -- foldl' recorre (de forma estricta) la lista de combinaciones de letra y posición,
  -- acumulando en cada paso (total, wm) donde:
  -- total: suma parcial de la puntuación de las letras.
  -- wm: multiplicador acumulado para el total de la palabra.
  foldl' acc (0, 1) (zip word positions)
  where
    -- Calcula las posiciones en el tablero donde se ubicará cada letra de la palabra
    positions = wordPositions start dir word brd
    
    -- Función de acumulación utilizada por foldl'.
    acc (total, wm) (chr, pos) =
        -- Se extraen los multiplicadores de letra (lm) y de palabra (wm')
        -- de la casilla en la posición pos obtenida del tablero.
        let Casilla { multiplicadorLetra = lm, multiplicadorPalabra = wm' } = brd Data.Array.! pos
        -- Se busca el valor de la letra `chr` en el mapa `tileValues`
        -- Si no se encuentra, se asume un valor por defecto de 0.
        in (total + (Map.findWithDefault 0 chr tileValues * lm), wm * wm')


-- Función que espera a que el usuario presione ENTER
waitForEnter :: IO ()
waitForEnter = do
  putStrLn "Presiona ENTER para continuar..."
  _ <- getLine
  return ()

-- Función auxiliar que intercambia todas las fichas del jugador con fichas nuevas de la bolsa.
exchangeTiles :: Int -> [Player] -> [Char] -> ([Player], [Char])
exchangeTiles idx players tileBag =
    let 
        -- Obtener el jugador actual a partir de su índice.
        current = players !! idx

        -- Convertir las fichas actuales del jugador (almacenadas en un Map de Char a Int)
        -- en una lista donde cada carácter se repite según su conteo.
        currentTiles = concatMap (\(c, n) -> replicate n c) (Map.toList (tiles current))

        -- Agregar las fichas actuales del jugador a la bolsa para que puedan ser reutilizadas.
        combinedBag = tileBag ++ currentTiles
        
        -- Determinar el número total de fichas que tiene el jugador.
        totalTiles = length currentTiles

        -- Dividir la bolsa combinada: se toman tantas fichas nuevas como tenía el jugador,
        -- y el resto permanece en la bolsa.
        (newTiles, remainingBag) = splitAt totalTiles combinedBag

        -- Construir un nuevo mapa que cuente cada ficha obtenida.
        newTileMap = Map.fromListWith (+) [(c, 1) | c <- newTiles]

        -- Actualizar el jugador con las fichas nuevas y reiniciar la bandera de pasar turno.
        updatedPlayer = current { tiles = newTileMap, passLastTurn = False }

        -- Reemplazar el jugador en la lista de jugadores con su versión actualizada.
        updatedPlayers = updateList idx updatedPlayer players
    in 
        (updatedPlayers, remainingBag)

        
--Lógica principal del juego: Función que gestiona el flujo del juego basado en el estado actual.
gameLoop :: GameState -> Trie Bool -> IO ()
gameLoop gameState@GameState{..} dictionary
    -- Caso base: Se verifica si se ha alcanzado el final del juego mediante la función checkGameEnd.
    | checkGameEnd gameState = do
        -- Se imprime el tablero junto con los movimientos realizados por los jugadores.
        printBoardWithMoves board players
        -- Se muestra un mensaje indicando que el juego ha terminado y se presentará la puntuación final.
        putStrLn "¡Juego terminado! Puntuación final:"
        -- Se recorre la lista de jugadores imprimiendo el nombre y la puntuación de cada uno.
        mapM_ (\p -> putStrLn $ name p ++ ": " ++ show (score p)) players
        -- Se determina el ganador comparando las puntuaciones de cada jugador.
        let winner = maximumBy (comparing score) players
        -- Se muestra el nombre del jugador ganador.
        putStrLn $ "¡Ganador: " ++ name winner ++ "!"
    -- Caso recursivo: El juego continúa.
    | otherwise = do
        -- Se limpia la consola para actualizar la vista.
        clearConsole
        -- Se extrae el jugador actual de la lista basándose en el índice currentPlayer.
        let Player{..} = players !! currentPlayer
        -- Se imprime el estado actual del tablero y los movimientos realizados.
        printBoardWithMoves board players
        -- Se indica de quién es el turno actual.
        putStrLn $ "\nTurno de: " ++ name
        -- Se muestran las fichas disponibles del jugador actual, convirtiendo el mapa de fichas a una lista.
        putStrLn $ "Fichas disponibles: " ++ show (Map.toList tiles)
        
        -- En el caso de bot:
        if take 3 name == "Bot"
            then do
                waitForEnter
                clearConsole 
                case findBestMove gameState dictionary of
                    Just (word, pos, dir, _) -> do
                        putStrLn $ "Bot juega: " ++ word ++ " en " ++ show pos ++ " " ++ show dir
                        handleMove pos dir word gameState dictionary
                    Nothing -> do
                        putStrLn "Bot opta por cambiar sus fichas"
                        let (exchangedPlayers, newBag) = exchangeTiles currentPlayer players tileBag
                            updatedPlayers = updateList currentPlayer ((exchangedPlayers !! currentPlayer) { passLastTurn = True }) exchangedPlayers
                            newState = gameState { players = updatedPlayers, tileBag = newBag }
                        -- let updatedPlayers = updateList currentPlayer (current { passLastTurn = True }) players
                        --     newState = gameState { players = updatedPlayers }
                        advanceTurn newState dictionary
            else do
                -- Muestra un mensaje solicitando al usuario que ingrese su movimiento.
                -- Se espera un movimiento con el formato: "x y H|V palabra" o la palabra "pasar" para cambiar fichas.
                putStrLn "Ingresa tu movimiento (formato: x y H|V palabra) o 'pasar':"
                hFlush stdout  -- Se vacía el buffer de salida para que el mensaje se muestre inmediatamente.
                input <- getLine  -- Se lee la entrada del usuario.
                if input == "pasar"
                    then do
                        -- Si el usuario ingresa "pasar", se procede a intercambiar las fichas.
                        putStrLn "Intercambiando fichas..."
                        -- Se llama a la función exchangeTiles para intercambiar las fichas del jugador actual.
                        let (updatedPlayers, newBag) = exchangeTiles currentPlayer players tileBag
                            -- Se crea un nuevo estado del juego actualizando la lista de jugadores y la bolsa de fichas.
                            newState = gameState { players = updatedPlayers, tileBag = newBag }
                        waitForEnter  -- Se espera a que el usuario presione ENTER.
                        advanceTurn newState dictionary  -- Se avanza al turno del siguiente jugador.
                    else case parseInput input of
                        -- Se intenta convertir la entrada del usuario al formato esperado.
                        Just ((x,y), dir, word) -> do
                            -- Si el parseo es exitoso, se llama a handleMove para procesar la jugada.
                            -- Se convierte la palabra a mayúsculas para estandarizar.
                            handleMove (x,y) dir (map toUpper word) gameState dictionary
                            waitForEnter  -- Se espera a que el usuario presione ENTER antes de continuar.
                        Nothing -> do
                            -- Si el formato de entrada es inválido, se notifica al usuario.
                            putStrLn "Formato inválido"
                            waitForEnter  -- Se espera a que el usuario presione ENTER.
                            gameLoop gameState dictionary  -- Se reinicia el ciclo del juego con el estado actual.

-- Manejo de movimientos con actualización de multiplicadores
-- Esta función se encarga de procesar un movimiento del jugador, validar que pueda realizarlo,
-- actualizar el tablero y el puntaje, reponer las fichas usadas y avanzar al siguiente turno.
handleMove :: (Int, Int) -> Direction -> String -> GameState -> Trie Bool -> IO ()
handleMove pos dir word gameState@GameState{..} dictionary = do
    -- Se obtiene el jugador actual de la lista de jugadores usando el índice currentPlayer.
    let current = players !! currentPlayer
    -- Verifica si el jugador tiene las fichas necesarias para formar la palabra.
    if not (hasTiles current word)
        then do
            -- Si no tiene suficientes fichas, se muestra un mensaje y se regresa al ciclo del juego.
            putStrLn "No tienes las fichas necesarias"
            gameLoop gameState dictionary
        else if not (isValidMove pos dir word gameState)
            then do
                -- Si el movimiento no es válido (por ejemplo, la posición ya está ocupada), se informa al jugador.
                putStrLn "Movimiento inválido"
                gameLoop gameState dictionary
            else do
                -- Si la jugada es válida, se procede a colocar la palabra y actualizar el estado del juego.
                let newBoard = fst (placeWord pos dir word board)
                    -- Se obtienen las palabras cruzadas que se forman tras colocar la palabra.
                    crossWords = findCrossWords pos dir word newBoard
                    -- Calcula el puntaje principal de la palabra colocada.
                    mainWordScore = calculateWordScore pos dir word newBoard
                    -- Calcula el puntaje total de las palabras cruzadas.
                    crossScores = sum [ calculateWordScore start d w newBoard | (start,d,w) <- crossWords ]
                    -- Se suma el puntaje principal y el de los cruces para obtener el puntaje total obtenido.
                    totalScore = mainWordScore + crossScores
                    -- Se actualiza el puntaje del jugador y se registra la jugada.
                    updatedPlayers = updatePlayerScore currentPlayer totalScore word players
                    -- Se reponen las fichas utilizadas, obteniendo la nueva bolsa y la lista actualizada de jugadores.
                    (newPlayers, newTileBag) = replenishTiles currentPlayer word updatedPlayers tileBag
                    -- Se construye el nuevo estado del juego con el tablero actualizado, la nueva lista de jugadores y la bolsa actualizada.
                    newState = gameState { board = newBoard
                                         , players = newPlayers
                                         , tileBag = newTileBag
                                         }
                -- Se muestra al jugador el puntaje obtenido en esta jugada.
                putStrLn $ "Puntuación obtenida: " ++ show totalScore
                -- Se avanza al turno del siguiente jugador con el estado actualizado.
                advanceTurn newState dictionary

-- Cálculo de puntuación con multiplicadores
-- Esta función calcula el puntaje total de una palabra al colocarla en el tablero,
-- teniendo en cuenta el valor de cada letra y los multiplicadores de la casilla.
calculateWordScore :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla -> Int
calculateWordScore (x,y) dir word board = 
    let 
        -- Se obtienen las posiciones en el tablero donde la palabra ocupará espacio.
        positions = wordPositions (x,y) dir word board
        -- Se empareja cada letra de la palabra con su posición correspondiente.
        letters = zip word positions
        -- Se procesa cada letra para acumular el puntaje base y los multiplicadores de palabra.
        (letterScores, wordMultipliers) = foldl' processLetter (0, 1) letters
    in 
        -- Se multiplica la suma del puntaje de las letras por el multiplicador global de la palabra.
        letterScores * wordMultipliers
  where
    -- Función auxiliar que, dada una letra y su posición, actualiza el acumulado del puntaje y
    -- del multiplicador global de la palabra.
    processLetter (total, wm) (c, pos) =
        let 
            -- Se extraen los multiplicadores de la casilla: lm para la letra y wm' para la palabra.
            Casilla _ lm wm' = board Data.Array.! pos
            -- Se calcula el valor de la letra multiplicado por su multiplicador de casilla.
            letterValue = Map.findWithDefault 0 c tileValues * lm
        in 
            -- Se actualiza el total sumando el valor obtenido y se acumula el multiplicador de palabra.
            (total + letterValue, wm * wm')

-- Reposición de fichas
-- La función replenishTiles actualiza las fichas de un jugador después de haber jugado una palabra.
-- Toma el índice del jugador, la palabra jugada, la lista de jugadores y la bolsa de fichas nuevas.
-- Devuelve la lista de jugadores actualizada y la bolsa de fichas restante.
replenishTiles :: Int -> String -> [Player] -> [Char] -> ([Player], [Char])
replenishTiles playerIndex word players newTiles =
    let 
        -- Se obtiene el jugador actual a partir de su índice.
        current = players !! playerIndex

        -- Se actualiza el conjunto de fichas del jugador eliminando cada ficha que se ha usado en 'word'.
        -- Para cada letra en 'word', se reduce su cuenta en el mapa de fichas.
        remainingTiles = foldl' (\m c -> Map.update (\v -> if v > 1 then Just (v-1) else Nothing) c m) (tiles current) word

        -- Se calcula cuántas fichas necesita el jugador para reponer hasta llegar a 7.
        tilesNeeded = 7 - Map.foldr (+) 0 remainingTiles

        -- Se extraen de la bolsa 'newTiles' las fichas necesarias para reponer.
        (newLetters, remainingBag) = splitAt tilesNeeded newTiles

        -- Se actualiza el mapa de fichas del jugador combinando las fichas restantes con las nuevas fichas obtenidas.
        updatedTiles = Map.unionWith (+) remainingTiles (Map.fromListWith (+) [(c,1) | c <- newLetters])

        -- Se reemplaza el jugador en la lista de jugadores con la versión actualizada.
        updatedPlayers = updateList playerIndex (current { tiles = updatedTiles }) players
    in 
        -- Se devuelve la lista actualizada de jugadores y la bolsa de fichas restante.
        (updatedPlayers, remainingBag)

-- La función 'placeWord' inserta una palabra en el tablero, actualizando las casillas correspondientes
-- y devolviendo el tablero actualizado junto con una lista de caracteres. 

placeWord :: (Int, Int)                     -- Coordenadas iniciales (x,y) donde se colocará la palabra
          -> Direction                      -- Dirección en la que se ubicará la palabra (por ejemplo, horizontal o vertical)
          -> String                         -- La palabra a insertar en el tablero
          -> Array (Int, Int) Casilla       -- El tablero actual representado como un array de casillas
          -> (Array (Int, Int) Casilla, [Char]) -- Resultado: el tablero actualizado y la lista de letras utilizadas
placeWord (x,y) dir word board = 
    (board Data.Array.// updates, map fst letters)  -- Actualiza el tablero con 'updates' y extrae las letras de 'letters'
  where
    -- Calcula las posiciones en el tablero donde se ubicará cada letra de la palabra
    positions = wordPositions (x,y) dir word board

    -- Asocia cada letra de la palabra con su correspondiente posición en el tablero
    letters = zip word positions

    -- Define las actualizaciones a aplicar: para cada posición, coloca la letra con multiplicadores 1 (por simplicidad)
    updates = [ (pos, Casilla (Just c) 1 1) | (c, pos) <- letters ]

-- La función `parseInput` se encarga de analizar una cadena de entrada y convertirla en una estructura de datos
-- que contiene una coordenada (tupla de dos enteros), una dirección (Horizontal o Vertical) y una palabra.
parseInput :: String -> Maybe ((Int, Int), Direction, String)
parseInput input = case words input of
    -- Se espera que la entrada se divida en exactamente cuatro partes: dos números, un indicador de dirección y una palabra.
    [x, y, dir, word] -> do
        -- Se intenta convertir el primer elemento x a un número entero.
        xNum <- readMaybe x
        -- Se intenta convertir el segundo elemento y a un número entero.
        yNum <- readMaybe y
        -- Se determina la dirección: "H" se convierte en Horizontal, "V" en Vertical; cualquier otro valor es inválido.
        direction <- case dir of
            "H" -> Just Horizontal
            "V" -> Just Vertical
            _   -> Nothing
        -- Se devuelve la tupla con la coordenada, la dirección y la palabra, encapsulada en el constructor Maybe.
        return ((xNum, yNum), direction, word)
    -- Si la entrada no tiene exactamente cuatro elementos, se devuelve Nothing (indicando un error de parseo).
    _ -> Nothing

-- Función updateList:
-- Actualiza la lista reemplazando el elemento en la posición 'n' por 'newVal'.
-- Toma los primeros 'n' elementos, agrega el nuevo valor y concatena el resto,
-- descartando el elemento que originalmente estaba en la posición 'n'.
updateList :: Int -> a -> [a] -> [a]
updateList n newVal list = take n list ++ newVal : drop (n + 1) list

-- Función advanceTurn:
-- Avanza al siguiente turno del juego actualizando el índice del jugador actual.
-- Se calcula el próximo índice usando (currentPlayer + 1) `mod` (número total de jugadores)
-- para reiniciar al primer jugador al llegar al final de la lista.
-- Luego, se llama recursivamente a 'gameLoop' con el estado del juego actualizado.
advanceTurn :: GameState -> Trie Bool -> IO ()
advanceTurn gameState@GameState{..} dictionary =
    gameLoop gameState { currentPlayer = (currentPlayer + 1) `mod` length players } dictionary



-- La función checkGameEnd determina si el juego debe finalizar
checkGameEnd :: GameState -> Bool
checkGameEnd GameState{..} = 
    -- Se verifica si el juego ha terminado, lo cual ocurre si:
    -- 1. La bolsa de baldosas (tileBag) está vacía Y alguno de los jugadores no tiene baldosas (tiles)
    -- 2. O si todos los jugadores pasaron en el turno anterior (getPassLastTurn devuelve todos True)
    (null tileBag && any (null . tiles) players)
    || 
    foldr (&&) True (getPassLastTurn players)

-- La función getPassLastTurn extrae en una lista el estado de "passLastTurn" de cada jugador.
getPassLastTurn :: [Player] -> [Bool]
getPassLastTurn players = [x | Player{ passLastTurn = x } <- players]



-- Función que obtiene posiciones válidas para colocar la palabra en el tablero.
getValidPositions :: GameState -> String -> [((Int,Int),Direction)]
getValidPositions GameState{ board = brd } word =
  [ ((x, y), dir)
  -- Iteración en todas las coordenadas x en el rango de 0 hasta boardSize - 1.
  | x <- [0 .. boardSize - 1]
  -- Iteración en todas las coordenadas y en el mismo rango.
  , y <- [0 .. boardSize - 1]
  -- Se prueban dos direcciones para la palabra: Horizontal y Vertical.
  , dir <- [Horizontal, Vertical]
  -- Verificar que la palabra cabe en el tablero comenzando en (x,y) en la dirección indicada.
  , fits (x, y) dir (length word)
  -- Comprobar que todas las posiciones que ocupará la palabra están vacías.
  , all isEmpty (wordPositions (x, y) dir word brd)
  ]
  where
    -- Función auxiliar que verifica si la palabra cabe en el tablero según la dirección.
    fits (x, _) Horizontal l = x + l <= boardSize
    fits (_, y) Vertical   l = y + l <= boardSize
    -- Función auxiliar que determina si una posición está vacía.
    isEmpty pos = isNothing (contenido (brd Data.Array.! pos))


-- Función que verifica si una jugada es válida, comprobando que todas las posiciones necesarias estén vacías.
isValidMove :: (Int, Int) -> Direction -> String -> GameState -> Bool
isValidMove pos dir word GameState{..} =
    let 
        -- Calcula las posiciones en las que se ubicará cada letra de la palabra.
        positions = wordPositions pos dir word board
    in 
        -- Verifica que cada posición indicada esté vacía (esto se asume con que 'contenido' retorne Nothing).
        all (\p -> contenido (board Data.Array.! p) == Nothing) positions

-- Actualiza el puntaje y registra la jugada (palabra y puntos obtenidos)
-- Recibe el índice del jugador en la lista, los puntos obtenidos, la palabra jugada y la lista de jugadores.
updatePlayerScore :: Int -> Int -> String -> [Player] -> [Player]
updatePlayerScore idx points playedWord players =
  updateList idx updatedPlayer players  -- Actualiza el jugador en la posición 'idx' de la lista con 'updatedPlayer'
  where
    player = players !! idx  -- Obtiene el jugador actual a partir del índice
    -- Crea una versión actualizada del jugador, sumando los puntos actuales y registrando el movimiento
    updatedPlayer = player { score = score player + points
                           , moves = moves player ++ [(playedWord, points)]
                           , passLastTurn = False  -- Reinicia el indicativo de pasar turno del jugador
                           }

-- Calcula el puntaje de una palabra interseccional en el tablero
-- Recibe una posición (fila, columna), la dirección de la palabra, la palabra y el tablero representado con un Array.
calculateCrossWordScore :: (Int, Int) -> Direction -> String -> Array (Int, Int) Casilla -> Int
calculateCrossWordScore pos dir word board = calculateWordScore pos dir word board
  -- Simplemente delega el cálculo del puntaje a la función 'calculateWordScore'


main :: IO ()
main = do
    putStrLn "Cargando diccionario..."
    dictContent <- readFile "dic/diccionario_filtrado.txt"
    let dictionary = T.fromList [ (C8.pack (map toUpper w), True) 
                                  | w <- lines dictContent ]
    gameState <- initializeGame
    gameLoop gameState dictionary




-- Esta función devuelve la dirección perpendicular a la dada. Es útil para determinar
-- la dirección opuesta (por ejemplo, en un juego de palabras cruzadas, cambiar de horizontal a vertical)
perpDirection :: Direction -> Direction
perpDirection Horizontal = Vertical  -- Si la dirección es Horizontal, la perpendicular es Vertical.
perpDirection Vertical   = Horizontal  -- Si la dirección es Vertical, la perpendicular es Horizontal.

-- Esta función extrae la palabra cruzada presente en el tablero a partir de una posición y dirección especificadas.
extractCrossWord :: (Int, Int) -> Direction -> Array (Int, Int) Casilla -> String
extractCrossWord pos dir board = reverse left ++ middle ++ right
  where
    -- 'left' contiene los caracteres de la palabra en la dirección contraria a la dada.
    -- Se utiliza 'takeWhile' para coger los caracteres hasta encontrar un espacio (' ')
    -- y 'drop 1' para omitir la posición inicial.
    left  = takeWhile (/= ' ') $ drop 1 $ map letterAt $ iterate (moveNeg dir) pos

    -- 'middle' obtiene el carácter de la posición dada.
    -- Se verifica si el índice es válido y se extrae el contenido de la casilla.
    -- Si no hay contenido, se usa un espacio.
    middle = case if isValidIndex pos board then contenido (board Data.Array.! pos) else Nothing of
               Just c -> [c]
               Nothing -> " "

    -- 'right' contiene los caracteres de la palabra en la dirección indicada,
    -- deteniéndose al llegar a un espacio (' ').
    right = takeWhile (/= ' ') $ map letterAt $ iterate (move dir) pos

    -- La función 'letterAt' retorna el carácter en una posición.
    -- Si la posición es válida, retorna el contenido de la casilla o un espacio en su defecto.
    letterAt p = if isValidIndex p board 
                 then fromMaybe ' ' (contenido (board Data.Array.! p))
                 else ' '

    -- Función para mover una posición en la dirección especificada.
    move Horizontal (x,y) = (x+1, y)  -- Si la dirección es Horizontal, incrementamos x.
    move Vertical   (x,y) = (x, y+1)    -- Si la dirección es Vertical, incrementamos y.
    
    -- Función para mover en la dirección contraria (negativa).
    moveNeg Horizontal (x,y) = (x-1, y)  -- Si la dirección es Horizontal, decrementamos x.
    moveNeg Vertical   (x,y) = (x, y-1)    -- Si la dirección es Vertical, decrementamos y.

-- La función isValidIndex verifica si una posición dada (tipo (Int, Int))
-- se encuentra dentro de los límites (bounds) de un Array.
-- inRange toma los límites del array (obtenidos con bounds arr) y la posición,
-- y retorna True si la posición está dentro del rango, o False en caso contrario.
isValidIndex :: (Int, Int) -> Array (Int, Int) a -> Bool
isValidIndex pos arr = inRange (bounds arr) pos

-- La función hasTiles determina si un jugador tiene suficientes fichas
-- para formar una palabra dada.
-- Parámetros:
--   player: estructura que contiene al menos un campo 'tiles',
--           el cual es un Map que relaciona cada caracter con la cantidad de fichas disponibles.
--   word: la palabra que queremos formar.
--
-- El funcionamiento es el siguiente:
--   1. nub word: se genera una lista con cada carácter único de la palabra.
--   2. all (\c -> ...): se verifica que para cada carácter c en esa lista,
--      el número de fichas disponibles para c (obtenido con Map.findWithDefault 0 c (tiles player))
--      sea mayor o igual que el número de veces que aparece c en la palabra.
--   3. La función filter (== c) word cuenta cuántas veces aparece el carácter c en la palabra.
hasTiles :: Player -> String -> Bool
hasTiles player word =
    all (\c -> Map.findWithDefault 0 c (tiles player) >= length (filter (== c) word))
        (nub word)

-- La función 'printBoardWithMoves' recibe un tablero (Array) y una lista de jugadores.
-- Imprime en la consola el tablero junto con una tabla que muestra las jugadas de cada jugador.
printBoardWithMoves :: Array (Int, Int) Casilla -> [Player] -> IO ()
printBoardWithMoves board players = do
  -- Imprime la cabecera del tablero que muestra los números de las columnas.
  -- Se utiliza 'boardSize' para determinar el tamaño del tablero.
  putStrLn $ "\n    " ++ concat [showCol i | i <- [0..boardSize-1]]

  -- Itera sobre cada fila del tablero.
  forM_ [0..boardSize-1] $ \r -> do
    -- Crea la lista 'row' obteniendo cada casilla usando la posición (x, r).
    let row = [ board ! (x, r) | x <- [0..boardSize-1] ]
        -- Concatena cada casilla formateada en un string.
        -- Se pone un espacio extra a la izquierda si el número de fila es menor a 10 para alinear.
        rowStr = (if r < 10 then " " ++ show r else show r) ++ "  " ++ concatMap showCasilla row
    -- Imprime la fila actual del tablero.
    putStrLn rowStr

  -- Después de imprimir el tablero, se imprime la sección de jugadas.
  putStrLn "\nJugadas:"

  -- Se extraen los dos primeros jugadores de la lista.
  -- Si la lista está vacía o no tiene suficientes jugadores, se lanza un error.
  let player1 = case players of
                 (p:_) -> p
                 []    -> error "No hay jugador 1"
      player2 = if length players > 1 then players !! 1 else error "No hay jugador 2"

      -- 'movesStr' formatea las jugadas de un jugador, mostrando la palabra y sus puntos entre paréntesis.
      movesStr p = [ w ++ "(" ++ show pts ++ ")" | (w, pts) <- moves p ]
      moves1 = movesStr player1  -- Jugadas del primer jugador.
      moves2 = movesStr player2  -- Jugadas del segundo jugador.
      nrows = max (length moves1) (length moves2)  -- Número de filas para la tabla de jugadas, basado en el jugador con más jugadas.
      -- Se rellenan las listas de jugadas para igualar el número de filas.
      padded1 = moves1 ++ replicate (nrows - length moves1) ""
      padded2 = moves2 ++ replicate (nrows - length moves2) ""
      colWidth = 30  -- Ancho fijo de la columna para alinear la salida.

  -- Imprime la cabecera de la tabla de jugadas con los nombres de los jugadores.
  -- Se añade espacios para que el nombre del primer jugador tenga el ancho 'colWidth'.
  putStrLn $ (name player1) ++ replicate (colWidth - length (name player1)) ' ' ++ (name player2)

  -- Imprime cada línea de jugadas, alineando ambas columnas.
  forM_ (zip padded1 padded2) $ \(l1, l2) -> do
    let s1 = l1 ++ replicate (colWidth - length l1) ' '  -- Rellena la cadena de la primera columna hasta 'colWidth'.
    putStrLn $ s1 ++ l2
  where
    -- 'showCol' es una función auxiliar para formatear los números de columna.
    showCol :: Int -> String
    showCol n
      | n < 10    = "  " ++ show n ++ "  "  -- Para números de una sola cifra, se agregan espacios extras.
      | otherwise = " "  ++ show n ++ "  "  -- Para números de dos dígitos, se utiliza un espacio menos.
    
    -- Función que formatea cada casilla del tablero
showCasilla :: Casilla -> String
showCasilla Casilla{..}
  | isJust contenido       = "  " ++ [fromJust contenido] ++ "  "
  | multiplicadorPalabra > 1 = " TW  "  -- Triple Palabra, por ejemplo
  | multiplicadorLetra > 1   = if multiplicadorLetra == 3 then " TL  " else " DL  "
  | otherwise              = "  .  "
