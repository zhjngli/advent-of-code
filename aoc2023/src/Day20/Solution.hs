{-# LANGUAGE TupleSections #-}
module Day20.Solution (solve) where

import Text.ParserCombinators.Parsec
    ( char,
      letter,
      newline,
      string,
      endBy1,
      many1,
      sepBy1,
      (<|>),
      parse,
      try,
      CharParser )
import qualified Data.Map.Strict as M
import Control.Monad.State.Strict

solve :: IO ()
solve = do
    input <- readFile "./src/Day20/input.txt"
    putStrLn $ "2023.20.1: " ++ show (solve1 $ parseInput input)
    putStrLn $ "2023.20.2: " ++ show (solve2 $ parseInput input)

data Signal = H | L deriving (Show, Eq)
data Module = B [String] | F Bool [String] | C (M.Map String Signal) [String] deriving (Show, Eq)

flipModule :: CharParser st (String, Module)
flipModule = do
    _ <- char '%'
    name <- many1 letter
    _ <- string " -> "
    outs <- sepBy1 (many1 letter) (string ", ")
    return (name, F False outs)

conModule :: CharParser st (String, Module)
conModule = do
    _ <- char '&'
    name <- many1 letter
    _ <- string " -> "
    outs <- sepBy1 (many1 letter) (string ", ")
    return (name, C M.empty outs) -- fill out inputs later

broadcastModule :: CharParser st (String, Module)
broadcastModule = do
    name <- string "broadcaster"
    _ <- string " -> "
    outs <- sepBy1 (many1 letter) (string ", ")
    return (name, B outs)

allModules :: CharParser st [(String, Module)]
allModules = endBy1 (try flipModule <|> try conModule <|> try broadcastModule) newline

parseInput :: String -> M.Map String Module
parseInput i = case parse allModules "" i of
    Left e -> error (show e)
    Right ms ->
        let cons = filter
                (\(_, m) -> case m of
                    C _ _ -> True
                    _ -> False
                ) ms
            ins = map
                (\(c, m) -> (c, m,
                    foldr (
                        \(n, m') is ->
                            case m' of
                            B outs -> if c `elem` outs then "broadcaster":is else is
                            F _ outs -> if c `elem` outs then n:is else is
                            C _ outs -> if c `elem` outs then n:is else is
                    ) [] ms
                    )
                ) cons
            consWithInputs = map
                (\(c, m, is) ->
                    case m of
                    C _ outs -> (c, C (M.fromList (map (, L) is)) outs)
                    _ -> error ("should be a cons module: " ++ c)
                ) ins
        in
        M.fromList (ms ++ consWithInputs)

-- output high pulses, low pulses, new modules
pressButton :: (Int, Int, M.Map String Module) -> (Int, Int, M.Map String Module)
pressButton (h, l, ms) = pulse ms [("button", L, "broadcaster")] h l

pulse :: M.Map String Module -> [(String, Signal, String)] -> Int -> Int -> (Int, Int, M.Map String Module)
pulse ms [] h l = (h, l, ms)
pulse ms ((from, H, name):ps) h l =
    case ms M.!? name of
    Just (B outs) -> pulse ms (ps ++ map (name, H,) outs) (h+1) l
    Just (F _ _) -> pulse ms ps (h+1) l
    Just (C ins outs) ->
        let ins' = M.insert from H ins
            ms' = M.insert name (C ins' outs) ms
            s = if all (== H) (M.elems ins') then L else H
        in
        pulse ms' (ps ++ map (name, s,) outs) (h+1) l
    Nothing -> pulse ms ps (h+1) l
pulse ms ((from, L, name):ps) h l =
    case ms M.!? name of
    Just (B outs) -> pulse ms (ps ++ map (name, L,) outs) h (l+1)
    Just (F True outs) -> pulse (M.insert name (F False outs) ms) (ps ++ map (name, L,) outs) h (l+1)
    Just (F False outs) -> pulse (M.insert name (F True outs) ms) (ps ++ map (name, H,) outs) h (l+1)
    Just (C ins outs) ->
        let ins' = M.insert from L ins
            ms' = M.insert name (C ins' outs) ms
            s = if all (== H) (M.elems ins') then L else H
        in
        pulse ms' (ps ++ map (name, s,) outs) h (l+1)
    Nothing -> pulse ms ps h (l+1)

solve1 :: M.Map String Module -> Int
solve1 ms = h * l
    where (h, l, _) = iterate pressButton (0, 0, ms) !! 1000

data ModuleState = ModuleState {
    modules :: M.Map String Module,
    buttonPresses :: Int,
    pulses :: [(String, Signal, String)],
    conIntoRx :: String,
    modsIntoCon :: M.Map String Int
} deriving Show

foundCycle :: State ModuleState Bool
foundCycle = get >>= \ms -> pure (all (> 0) (M.elems $ modsIntoCon ms))
    -- do
    -- ms <- get
    -- return $ all (> 0) (M.elems $ modsIntoCon ms)

-- some copy/pasta cause i'm too lazy to refactor this properly
singlePulse :: M.Map String Module -> [(String, Signal, String)] -> ([(String, Signal, String)], M.Map String Module)
singlePulse ms [] = ([], ms)
singlePulse ms ((from, H, name):ps) =
    case ms M.!? name of
    Just (B outs) -> (ps ++ map (name, H,) outs, ms)
    Just (F _ _) -> (ps, ms)
    Just (C ins outs) ->
        let ins' = M.insert from H ins
            ms' = M.insert name (C ins' outs) ms
            s = if all (== H) (M.elems ins') then L else H
        in
        (ps ++ map (name, s,) outs, ms')
    Nothing -> (ps, ms)
singlePulse ms ((from, L, name):ps) =
    case ms M.!? name of
    Just (B outs) -> (ps ++ map (name, L,) outs, ms)
    Just (F True outs) -> (ps ++ map (name, L,) outs, M.insert name (F False outs) ms)
    Just (F False outs) -> (ps ++ map (name, H,) outs, M.insert name (F True outs) ms)
    Just (C ins outs) ->
        let ins' = M.insert from L ins
            ms' = M.insert name (C ins' outs) ms
            s = if all (== H) (M.elems ins') then L else H
        in
        (ps ++ map (name, s,) outs, ms')
    Nothing -> (ps, ms)

stPressButton :: State ModuleState ()
stPressButton = do
    ms <- get
    let ps = pulses ms
    let con = conIntoRx ms
    let n = if null ps then buttonPresses ms + 1 else buttonPresses ms
    let (ps', ms') =
            if null ps then singlePulse (modules ms) [("button", L, "broadcaster")]
            else singlePulse (modules ms) ps
    let ins = case ms' M.! con of
            C i _ -> i
            _ -> error (show con ++ " should be a cons module with stored inputs")
    put ms {
        modules = ms',
        pulses = ps',
        buttonPresses = n,
        modsIntoCon = M.foldlWithKey'
            (\acc name sig ->
                -- trace ("in: " ++ name ++ ", sig: " ++ show sig) $
                if sig == H && acc M.! name == 0 then M.insert name n acc
                else acc
            )
            (modsIntoCon ms) ins
    }
    return ()

search :: ModuleState -> Int
search ms
    | evalState foundCycle ms = M.foldl' lcm 1 (modsIntoCon ms)
    | otherwise = search $ execState stPressButton ms

solve2 :: M.Map String Module -> Int
solve2 ms = search initState
    where
        con =
            let candidates = M.foldlWithKey'
                    (\acc name m ->
                        case m of
                        C _ outs -> if "rx" `elem` outs then name:acc else acc
                        _ -> acc
                    )
                    [] ms
            in
            case candidates of
            [c] -> c
            _ -> error ("too many candidates for cons modules into rx: " ++ show candidates)
        initState = ModuleState {
            modules = ms,
            buttonPresses = 0,
            pulses = [],
            conIntoRx = con,
            modsIntoCon = case ms M.! con of
                C ins _ -> M.foldlWithKey' (\acc name _ -> M.insert name 0 acc) M.empty ins
                _ -> error ""
        }
