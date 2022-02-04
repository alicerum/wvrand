module Command.Dice (
    throwDice
) where

import Data.List
import Command.Internal.Parser
import System.Random
import System.IO (hPutStrLn, stderr)
import System.Exit (exitFailure)

type Dice = (Int, Int)

parseDice :: Parser Dice
parseDice = (,) <$> integer <* char 'd' <*> integer <* eof

genDiceThrows :: (RandomGen g) => g -> Dice -> [Int]
genDiceThrows g (amount, size) =
    take amount $ unfoldr (Just . uniformR (1, size)) g

throwDice :: String -> IO ()
throwDice dice = do
    case runParser parseDice dice of
        Nothing -> do
            hPutStrLn stderr $ "Wrong dice value '" <> dice <> "'"
            exitFailure
        Just (d, _) -> do
            gen <- newStdGen
            let dices = genDiceThrows gen d
            putStrLn $ show (sum dices) <> " <- " <> show dices

