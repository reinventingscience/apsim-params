{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
module APSIM.Client
  ( runAPSIMClient
  , defaultAPSIMServer
  , defaultAPSIMServerPort
  , APSIMSession(..)
  ) where

import Control.Category ((>>>))
import qualified Control.Exception as E
import Data.Binary.Get (getDoublele, getWord32le, isEmpty, runGet)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Char8 as C
import qualified Data.ByteString.Lazy as LBS
import Data.Dependent.Sum
import Data.Function ((&))
import Data.Functor.Identity (Identity(..))
import Data.Text (Text)
import qualified Data.Text.Encoding as BS
import qualified Data.Text.Lazy.Builder as TLB
import Data.Word (Word32)
import Formatting
import Network.Run.TCP (runTCPClient)
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendMany)
import System.IO (stderr)

import APSIM.Data (Variable(..), APSIMType(..), fAPSIMValue)
import APSIM.Decode

bufferSize :: Word32
bufferSize = 4096

defaultAPSIMServer :: HostName
defaultAPSIMServer = "127.0.0.1"

defaultAPSIMServerPort :: ServiceName
defaultAPSIMServerPort = "27746"

ackCommand, finCommand, runCommand, readCommand :: BS.ByteString
ackCommand = "ACK"
finCommand = "FIN"
runCommand = "RUN"
readCommand = "READ"

data APSIMSession = APSIMSession
  { sReportTableName :: Text
  , sReportVariables :: [Variable]
  } deriving (Eq, Show)

sVariableNames :: APSIMSession -> [Text]
sVariableNames session = vAddress <$> sReportVariables session

runAPSIMClient :: HostName -> ServiceName -> APSIMSession -> IO ()
runAPSIMClient host port session = do
  fprintLn "Connecting to APSIM server..."
  runTCPClient host port $ \s -> do
    fprintLn "Connected."
    sendRunWithAck s
    sendFinWithAck s
    finished <- readFromSocket s
    fprintLn ("Finished with status " % stext) $ BS.decodeUtf8 finished
    if finished == finCommand
      then readResults s session
      else return ()
    fprintLn "Done."

readResults :: Socket -> APSIMSession -> IO ()
readResults s session = do
  -- Tell the server we want to read the results
  sendRead s

  -- Tell it the table we want results from
  sendWithAck s (sReportTableName session)

  -- Send the parameters one at a time
  traverse (sendWithAck s) (sVariableNames session)
  sendFin s -- indicates the parameter list is complete

  finished <- readFromSocket s
  fprintLn ("Finished with status " % stext) $ BS.decodeUtf8 finished
  if finished == finCommand
    then do
      sendAck s
      traverse readResult (sReportVariables session)
      return ()
    else return ()

  where
    readResult variable = do
      result <- readFromSocket s
      sendAck s
      fprintLn ("Received data for variable '" % stext % "': '" % list fAPSIMValue % "'")
        (vAddress variable)
        (decodeVariables (vType variable) (LBS.fromStrict result))

class Sendable a where
  toPayload :: a -> BSB.Builder

instance Sendable BSB.Builder where
  toPayload = id

instance Sendable BS.ByteString where
  toPayload = BSB.byteString

instance Sendable Text where
  toPayload = BSB.byteString . BS.encodeUtf8

instance Sendable Int where
  toPayload = BSB.word32LE . fromIntegral

toMessage :: Sendable a => a -> BS.ByteString
toMessage = LBS.toStrict . BSB.toLazyByteString . toPayload

sendToSocket :: Sendable a => Socket -> a -> IO ()
sendToSocket s msg' =
  let msg = toMessage msg'
      len = BS.length msg & toMessage
  in do
    fprintLn ("Sending message with length " % int % ": '" % stext % "'") (BS.length msg) (BS.decodeASCII msg)
    sendMany s [len, msg]

sendWithAck :: Sendable s => Socket -> s -> IO ()
sendWithAck s cmd = do
  sendToSocket s cmd
  validateResponse s ackCommand

sendRunWithAck :: Socket -> IO ()
sendRunWithAck s = sendWithAck s runCommand

sendRead :: Socket -> IO ()
sendRead s = sendWithAck s readCommand

sendFin :: Socket -> IO ()
sendFin s = sendToSocket s finCommand

sendAck :: Socket -> IO ()
sendAck s = sendToSocket s ackCommand

sendFinWithAck :: Socket -> IO ()
sendFinWithAck s = sendWithAck s finCommand

readFromSocket :: Socket -> IO BS.ByteString
readFromSocket s = do
  lenBs <- recv s 4
  let len = runGet getWord32le (LBS.fromStrict lenBs) -- Assumes server machine is little-endian TODO: error handling
  fprintLn ("Receiving message with length " % int) len
  result <- receiveAll "" len
  -- fprintLn ("Received: '" % stext % "'") (BS.decodeUtf8 result)
  return result
  where
    receiveAll received pending =
      if pending == 0
        then return received
        else
          let toRead = min bufferSize pending
          in do
            chunk <- recv s (fromIntegral toRead)
            receiveAll (received <> chunk) (pending - toRead)

validateResponse :: Socket -> BS.ByteString -> IO ()
validateResponse s expected = do
  received <- readFromSocket s
  if received == expected
    then fprintLn ("Received expected response '" % stext % "'") $ BS.decodeUtf8 expected
    else hprintLn stderr ("Unexpected response! Expected '" % stext % "', received '" % stext % "'.") (BS.decodeUtf8 expected) (BS.decodeUtf8 received)
    -- TODO: actual errors...
