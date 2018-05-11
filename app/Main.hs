module Main where
import qualified Midi
import qualified Args

main :: IO ()
main = do
  args <- Args.getArgs
  out <- Midi.parseMidi args
  mapM_ putStrLn out
