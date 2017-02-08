import Criterion.Main
import Pipes
import qualified Pipes.Prelude as P
import Pipes.Merge

-- Our benchmark harness.
main = defaultMain [
  bench "unbuffered" $ nfIO (P.sum (each [1..1000] >-> P.map (+1) >-> P.map (`div` 2)) :: IO Int),
  bench "buffered" $ nfIO (P.sum (each [1..1000] >-> steal 1 [P.map (+1)] >-> steal 1 [P.map (`div` 2)]) :: IO Int)
  ]

-- Better: xor together streams of random numbers.
-- Compare: deterministic using P.zipWith
-- Work stealing, with a consumer that reads to and xors.
-- Shows overhead when no buffering happens.
--
-- This treats Requests and Responses the same. We should privilidge
-- responses. We want to buffer as little as possible. 
-- So our queue will only contain responses. There will be a single
-- mvar for requests, using throughVar to send a response
-- back to the blocked thread.
--
-- For broadcasts, Can we use a broadcast channel instead of iterating?
-- We statically create a broadcast channel. Also a barrier: an mvar int.
-- When the mvar count == num workers, mvar is reset to 0, the mvar for
-- requests is set to h resonse that writes to the broadcast channel.
