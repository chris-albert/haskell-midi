module Main where
import qualified Midi
import qualified Args

main :: IO ()
main = do
  args <- Args.getArgs
  out <- Midi.parseMidi args
  mapM_ print out

renderChunk :: Either String [Midi.Chunk] -> [String]
renderChunk (Right cs) = show <$> cs
renderChunk (Left e)   = ["Error: " ++ e]

