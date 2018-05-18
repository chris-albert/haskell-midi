module Event where

import qualified Data.ByteString.Lazy as BSL
import qualified Data.ByteString as BS
import Data.Word

data Event = 
  MidiEvent 
    {
      deltaTime :: Word32,
      midiMessage :: MidiMessage
    }
  |
  SysexEvent 
    {
      deltaTime :: Word32,
      sysexData :: BS.ByteString
    }
  | 
  MetaEvent 
    {
      deltaTime :: Word32,
      metaMessage :: MetaMessage
    }
  deriving (Show)

data MidiMessage =
  NoteOff 
    {
      channel  :: Word8,
      key      :: Word8,
      velocity :: Word8
    }
  |
  NoteOn
    {
      channel  :: Word8,
      key      :: Word8,
      velocity :: Word8
    }
  |
  PolyphonicKeyPressure
    {
      channel  :: Word8,
      key      :: Word8,
      pressure :: Word8
    }
  |
  ControllerChange 
    {
      channel          :: Word8,
      controllerNumber :: Word8,
      controllerValue  :: Word8
    }
  |
  ProgramChange
    {
      channel          :: Word8,
      newProgramNumber :: Word8
    }
  |
  ChannelKeyPressure
    {
      channel  :: Word8,
      pressure :: Word8
    }
  |
  PitchBend 
    {
      channel          :: Word8,
      leastSignificant :: Word8,
      mostSignificant  :: Word8
    }
  deriving (Show)

data ControllerNumber =
  AllSoundOff |
  ResetAllControllers |
  LocalControl
    {
      mode :: LocalControlMode
    }
  |
  AllNotesOff |
  OmniModeOff |
  OmniModeOn
    {
      numberChannels :: Word8
    }
  |
  PolyModeOn
  deriving (Show)

data LocalControlMode = Disconnect | Reconnect deriving (Show)


data MetaMessage = 
  SequenceNumber 
    {
      sequence :: Word16
    }
  |
  TextEvent
    {
      text :: BS.ByteString
    }
  |
  CopyrightNotice
    {
      text :: BS.ByteString
    }
  |
  TrackName
    {
      text :: BS.ByteString
    }
  |
  InstrumentName
    {
      text :: BS.ByteString
    }
  |
  Lyric
    {
      text :: BS.ByteString
    }
  |
  Marker
    {
      text :: BS.ByteString
    }
  |
  CuePoint
    {
      text :: BS.ByteString
    }
  |
  MidiChannelPrefix
    {
      channelPrefix :: Word8
    }
  |
  EndOfTrack |
  SetTempo 
    {
      tempoInMicrosecondsAQuarerNote :: Word32
    }
  |
  SMTPEOffset
    {
      hours   :: Word8,
      minutes :: Word8,
      seconds :: Word8,
      frames  :: Word8,
      fractionalFrame :: Word8
    }
  |
  TimeSignature
    {
      numerator   :: Word8,
      denominator :: Word8,
      clocksPerMetronomeTick :: Word8,
      numberThirtySecondNotesPerTwentyFourMidiClocks :: Word8
    }
  |
  KeySignature
    {
      sharps :: Word8,
      gender :: Word8
    }
  |
  SequencerMetaEvent
    {
      body :: BS.ByteString
    }
  deriving (Show)


