module Bls.Run (
    runBls,
) where

import Bls.Scripts (blsAddScalarListScript, listOfSizedByteStrings)
import GHC.ByteOrder (ByteOrder (..))
import Plutus.Crypto.BlsUtils (mkScalar)
import PlutusTx.Builtins (byteStringToInteger)
import PlutusTx.Prelude (toBuiltin)

import PlutusBenchmark.Common (TestSize (..), printHeader, printSizeStatistics)

import System.IO (Handle)
import Text.Printf (hPrintf)

printCostsBls :: Handle -> Integer -> IO ()
printCostsBls h n =
    let script = blsAddScalarListScript . map (mkScalar . byteStringToInteger BigEndian . toBuiltin) $ listOfSizedByteStrings n 31
     in printSizeStatistics h (TestSize n) script

runBls :: Handle -> IO ()
runBls h = do
    hPrintf h "\n\n"

    hPrintf h "add a list of n scalars to each other (size 31 bytes) \n\n"
    printHeader h
    mapM_ (printCostsBls h) [2, 3, 4, 5, 6]
    hPrintf h "\n\n"
