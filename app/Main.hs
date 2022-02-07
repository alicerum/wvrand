module Main where

import Options.Applicative
import qualified Command.Bytes as Bytes
import qualified Command.Dice as Dice
import qualified Command.Password as Password

data Command = Bytes | LazyBytes | Password Int | Dice String deriving (Show)

newtype Options = Options Command deriving (Show)

bytesCommand :: Parser Command
bytesCommand = pure Bytes <**> helper

lazyBytesCommand :: Parser Command
lazyBytesCommand = pure LazyBytes <**> helper

diceCommand :: Parser Command
diceCommand = Dice <$> strOption (long "diceval"
                                <> short 'd'
                                <> help "Dice description, i.e. '1d6'") <**> helper

passwordCommand :: Parser Command
passwordCommand = Password <$> option auto (long "length"
                                           <> short 'l'
                                           <> help "Generated password length" ) <**> helper

readOptions :: Parser Options
readOptions = Options <$> subparser
            (  command "bytes"     (info bytesCommand     (progDesc "Output stream of uniformally distributed bytes"))
            <> command "lazybytes" (info lazyBytesCommand (progDesc "Output stream of uniformally distributed bytes generated lazily"))
            <> command "dice"      (info diceCommand      (progDesc "Output result of dice throw"))
            <> command "password"  (info passwordCommand  (progDesc "Random password of specified length")))

main :: IO ()
main = runProgram =<< execParser opts
  where
    opts = info (readOptions <**> helper)
      ( fullDesc
     <> progDesc "Generate some random data"
     <> header "wvrand generates random pass, dice or bytestream output" )

runProgram :: Options -> IO ()
runProgram opts = do
    case opts of
        Options Bytes -> Bytes.randomBytes
        Options LazyBytes -> Bytes.lazyBytes
        Options (Dice dice) -> Dice.throwDice dice
        Options (Password length) -> Password.genPass length

