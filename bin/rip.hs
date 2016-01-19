import qualified Sound.PortMidi as PM

import System.Environment
import System.Cmd
import System.Exit

import Data.Bits
import Control.Concurrent
import Text.Printf
import Foreign.C

channel = 1
notes = [12 .. 100]
time = 3

main =
   do args <- getArgs
      maybe usage start $ readMidiport args

start midiport = do PM.initialize
                    econn <- PM.openOutput midiport 1
                    either (handleOutput midiport) (handleFailure midiport) econn
                    PM.terminate
                    exitSuccess

usage = do
  putStrLn "Usage: tidal-rip <midiport-number>"
  availablePorts
  exitFailure

readMidiport :: [String] -> Maybe Int
readMidiport args | length args > 0 = Just $ read $ head args
                  | otherwise = Nothing

handleFailure i err = do
  putStrLn ("Error opening device " ++ (show i) ++ " (" ++ (show err) ++ ")\n")
  availablePorts
  exitFailure

availablePorts = do
  count <- PM.countDevices
  devices <- fmap (zip [0..]) $ mapM PM.getDeviceInfo [0..(count - 1)]
  putStrLn ("Available devices\n" ++ (unlines (["ID:\tName"]++(zipWith (++) (map (show . fst) devices) (map ((":\t"++) . PM.name . snd) devices)))))

handleOutput i o = do
  info <- PM.getDeviceInfo i
  putStrLn ("Opened: " ++ show (PM.interface info) ++ ": " ++ show (PM.name info))
  sequence_ $ map (play o) notes

play :: PM.PMStream -> Int -> IO ()
play conn n =
  do let tmpfn = printf "tmp-%03d.wav" n
         fn = printf "note-%03d.wav" n
     forkIO $ do rawSystem "ecasound" ["-t:" ++ (show time), "-i", "jack,system", "-o", tmpfn]
                 return ()
     threadDelay 500000
     noteOn conn channel (fromIntegral n) 80 0
     forkIO $ do threadDelay 50000
                 noteOff conn channel (fromIntegral n) 0
                 return ()
     threadDelay $ 1000000 * time
     forkIO $ do rawSystem "sox" [tmpfn, fn, "silence", "1", "0", "-55d", "reverse", "silence", "1", "0", "-55d", "reverse"]
                 rawSystem "rm" [tmpfn]
                 return ()
     return ()

-- MIDI Messages
noteOn :: PM.PMStream -> CLong -> CLong -> CLong -> CULong -> IO (PM.PMError)
noteOn o ch val vel t = do
  let evt = makeEvent 0x90 val ch vel t
  PM.writeEvents o [evt]

noteOff :: PM.PMStream -> CLong -> CLong -> CULong -> IO (PM.PMError)
noteOff o ch val t = do
  let evt = makeEvent 0x80 val ch 60 t
  PM.writeEvents o [evt]

-- MIDI Utils
encodeChannel :: (Bits a, Num a) => a -> a -> a
encodeChannel ch cc = (((-) ch 1) .|. cc)

makeEvent :: CLong -> CLong -> CLong -> CLong -> CULong -> PM.PMEvent
makeEvent st n ch v t = PM.PMEvent msg (t)
  where msg = PM.PMMsg (encodeChannel ch st) (n) (v)
