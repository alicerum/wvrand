module Command.Password (
    genPass
) where

import System.Random

data CharType = CapitalLetter
              | SmallLetter
              | Digit
              | Special
              deriving (Bounded, Enum, Show)

genSmallLetter :: (RandomGen g) => g -> (Char, g)
genSmallLetter = uniformR ('a', 'z')

genCapitalLetter :: (RandomGen g) => g -> (Char, g)
genCapitalLetter = uniformR ('A', 'Z')

genDigit :: (RandomGen g) => g -> (Char, g)
genDigit = uniformR ('0', '9')

genSpecial :: (RandomGen g) => g -> (Char, g)
genSpecial g = let pool = "!.,<>&*()$%#@^{}[]"
                   l = length pool
                   (i, g') = uniformR (0, l-1) g
               in (pool !! i, g')

genChar :: (RandomGen g) => CharType -> g -> (Char, g)
genChar CapitalLetter = genCapitalLetter
genChar SmallLetter = genSmallLetter
genChar Digit = genDigit
genChar Special = genSpecial

-- | Generates a list of Character types which we are going
-- to use in the future password, list of the desired length
genTypes :: (RandomGen g) => Int -> g -> ([CharType], g)
genTypes len g
    | len <= 0  = ([], g)
    | otherwise = let (t, g') = uniformR (min, max) g
                  in t `app` genTypes (len-1) g'
                  where
                      min = fromEnum (minBound :: CharType)
                      max = fromEnum (maxBound :: CharType)
                      app v (vs, g) = (toEnum v : vs, g)

-- | For every character type in the typelist generates a character
-- while using new seed for every new generated value
genCharsForTypes :: (RandomGen g) => [CharType] -> g -> (String, g)
genCharsForTypes [] g = ([], g)
genCharsForTypes (t : ts) g = let (c, g') = genChar t g
                              in c `ap` genCharsForTypes ts g'
                              where
                                  ap c (cs, g) = (c : cs, g)

genPassword :: (RandomGen g) => Int -> g -> (String, g)
genPassword len g = let (types, g') = genTypes len g
                    in genCharsForTypes types g'

-- | IO operation that creates the new seed, runs RNG functions
-- against that seed, and prints the result
genPass :: Int -> IO ()
genPass l = do
    gen <- newStdGen
    let (pass, _) = genPassword l gen
    putStrLn pass

