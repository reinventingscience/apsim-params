{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import qualified Control.Exception as E
import Data.Binary.Get (runGet, getWord32le)
import Data.Function ((&))
import Data.Word (Word32)
import qualified Data.ByteString.Builder as BSB
import qualified Data.ByteString.Lazy as LBS
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C
import qualified Data.Text.Lazy.Builder as TLB
import qualified Data.Text.Encoding as BS
import Formatting
import Network.Socket hiding (recv)
import Network.Socket.ByteString (recv, sendMany)
import Network.Run.TCP (runTCPClient)
import System.IO (stderr)

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

main :: IO ()
main = runAPSIMClient

runAPSIMClient :: IO ()
runAPSIMClient = do
  fprintLn "Connecting to APSIM server..."
  runTCPClient defaultAPSIMServer defaultAPSIMServerPort $ \s -> do
    fprintLn "Connected."
    sendRun s
    sendFin s
    finished <- readFromSocket s
    fprintLn ("Finished with status " % stext) $ BS.decodeUtf8 finished
    if finished == finCommand
      then readResults
      else return ()
    fprintLn "Done."

readResults :: IO ()
readResults = return () -- TODO

class Sendable a where
  toPayload :: a -> BSB.Builder

instance Sendable BSB.Builder where
  toPayload = id

instance Sendable BS.ByteString where
  toPayload = BSB.byteString

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

sendWithAck :: BS.ByteString -> Socket -> IO ()
sendWithAck cmd s = do
  sendToSocket s cmd
  validateResponse s ackCommand

sendRun :: Socket -> IO ()
sendRun = sendWithAck runCommand

sendFin :: Socket -> IO ()
sendFin = sendWithAck finCommand

readFromSocket :: Socket -> IO BS.ByteString
readFromSocket s = do
  lenBs <- recv s 4
  let len = runGet getWord32le (LBS.fromStrict lenBs) -- TODO: error handling
  fprintLn ("Receiving message with length " % int) len 
  receiveAll "" len
  where
    receiveAll received pending =
      if pending == 0
        then return received
        else
          let toRead = min bufferSize pending
          in do
            chunk <- recv s (fromIntegral toRead)
            receiveAll (chunk <> received) (pending - toRead)

validateResponse :: Socket -> BS.ByteString -> IO ()
validateResponse s expected = do
  received <- readFromSocket s
  if received == expected
    then fprintLn ("Received expected response '" % stext % "'") $ BS.decodeUtf8 expected
    else hprintLn stderr ("Unexpected response! Expected '" % stext % "', received '" % stext % "'.") (BS.decodeUtf8 expected) (BS.decodeUtf8 received)
    -- TODO: actual errors...
