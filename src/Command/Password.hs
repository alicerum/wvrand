module Command.Password (
    genPass
) where

import Control.Monad
import System.Random
import Command.Internal.State

data CharType = CapitalLetter
              | SmallLetter
              | Digit
              | Special
              deriving (Bounded, Enum, Show)

uniformSt :: (RandomGen g, Uniform a) => State g a
uniformSt = State uniform

uniformRSt :: (RandomGen g, UniformRange a) => (a, a) -> State g a
uniformRSt range = State (uniformR range)

genSmallLetter :: (RandomGen g) => State g Char
genSmallLetter = uniformRSt ('a', 'z')

genCapitalLetter :: (RandomGen g) => State g Char
genCapitalLetter = uniformRSt ('A', 'Z')

genDigit :: (RandomGen g) => State g Char
genDigit = uniformRSt ('0', '9')

genSpecial :: (RandomGen g) => State g Char
genSpecial = do
    let pool = "!.,<>&*()$%#@^{}[]"
        l = length pool
    i <- uniformRSt (0, l-1)
    return $ pool !! i

genChar :: (RandomGen g) => CharType -> State g Char
genChar CapitalLetter = genCapitalLetter
genChar SmallLetter = genSmallLetter
genChar Digit = genDigit
genChar Special = genSpecial

-- | Generates a list of Character types which we are going
-- to use in the future password, list of the desired length
genTypes :: (RandomGen g) => Int -> State g [CharType]
genTypes len = replicateM len (toEnum <$> uniformRSt (min, max))
               where
                   min = fromEnum (minBound :: CharType)
                   max = fromEnum (maxBound :: CharType)

-- | For every character type in the typelist generates a character
-- while using new seed for every new generated value
genCharsForTypes :: (RandomGen g) => [CharType] -> State g String
genCharsForTypes = mapM genChar

genPassword :: (RandomGen g) => Int -> State g String
genPassword = genCharsForTypes <=< genTypes

-- | IO operation that creates the new seed, runs RNG functions
-- against that seed, and prints the result
genPass :: Int -> IO ()
genPass l = do
    gen <- newStdGen
    let (pass, _) = runState (genPassword l) gen
    putStrLn pass

