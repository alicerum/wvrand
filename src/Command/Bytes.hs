module Command.Bytes (
    randomBytes,
    lazyBytes
) where

import System.Random
import System.Random.Stateful
import Data.Word
import qualified System.Random.MWC as MWC
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L
import Control.Monad

buildByteString :: (RandomGen g) => Int -> g -> [Word8] -> (L.ByteString, g)
buildByteString len g acc
    | len <= 0  = (L.pack acc, g)
    | otherwise = let (w, g') = uniform g
                  in buildByteString (len-1) g' (w : acc)

-- | Creates infinite amount of chunks of 1024 byte long byte strings and
-- appends them in a lazy way.
lazyByteStream :: (RandomGen g) => g -> L.ByteString
lazyByteStream g =
    let (bs, g') = buildByteString 1024 g []
    in bs `L.append` lazyByteStream g'

lazyBytes :: IO ()
lazyBytes = do
    gen <- newStdGen
    L.putStr $ lazyByteStream gen



-- | Uses MWC random number generator in order to produce 1024 byte long
-- byte strings and outputs them into the stdout. Unfortunately, MWC RNG
-- is Stateful only, and is tied to IO
bytesStream :: MWC.GenIO -> IO B.ByteString
bytesStream g = do
    bs <- B.pack <$> uniformListM 1024 g
    B.putStr bs
    bytesStream g

randomBytes :: IO ()
randomBytes = do
    gen <- MWC.createSystemRandom
    bytesStream gen
    return ()

