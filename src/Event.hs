module Event where

import qualified Data.ByteString.Lazy as BSL
import Data.Word

data Event = 
  MidiEvent 
    {
      midiMessage :: MidiMessage
    }
  | 
  SysexEvent 
    {
      length    :: Word32,
      sysexData :: BSL.ByteString
    }
  | 
  MetaEvent 
    {
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
      text :: BSL.ByteString
    }
  |
  CopyrightNotice
    {
      text :: BSL.ByteString
    }
  |
  TrackName
    {
      text :: BSL.ByteString
    }
  |
  InstrumentName
    {
      text :: BSL.ByteString
    }
  |
  Lyric
    {
      text :: BSL.ByteString
    }
  |
  Marker
    {
      text :: BSL.ByteString
    }
  |
  CuePoint
    {
      text :: BSL.ByteString
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
      id :: Word32,
      body :: BSL.ByteString
    }
  deriving (Show)


