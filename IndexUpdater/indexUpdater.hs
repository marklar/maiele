module Main where

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad (forever)
import Text.ParserCombinators.Parsec

import Message
import MsgParser (msgJson)


-- CONFIG --
connIP = "127.0.0.1"
vHost = "/"
user = "guest"
pwd = "guest"
xName = "mark.glu_stock"
xType = "fanout"
qName = "mark.glu_stock_queue_lsyncd"
routingKey = ""


main = do
  conn <- setUpAndSubscribe
  waitAround
  -- never gets here...
  closeConnection conn
  putStrLn "connection closed"

-----

setUpAndSubscribe :: IO Connection
setUpAndSubscribe = do
  conn <- openConnection connIP vHost user pwd
  chan <- openChannel conn
  declareExchange chan newExchange { exchangeName = xName
                                   , exchangeType = xType
                                   }
  declareQueue chan newQueue { queueName = qName }
  bindQueue chan qName xName routingKey
  -- subscribe
  consumeMsgs chan qName Ack myCallback
  return conn

waitAround :: IO ()
waitAround =
    forever $ do
      return ()

-----
    
myCallback :: (Message,Envelope) -> IO ()
myCallback (msg, env) = do
  let body = BL.unpack $ msgBody msg

  case parse msgJson "error" body of
    Left e ->
        logError e body
    Right json -> do
        logChange json
        updateIndex $ getMsgData json

  -- acknowledge receiving the message
  ackEnv env

updateIndex :: Msg -> IO ()
updateIndex (Msg _ _ (Just _))      = do
  putStrLn "Nothing to do."
updateIndex (Msg gluId isInStock _) = do
  putStrLn $ "gluId:     " ++ show gluId
  putStrLn $ "isInStock: " ++ show isInStock
  -- ADD CODE HERE HERE HERE.

-- LOGGING --

logError :: ParseError -> String -> IO ()
logError e body = do
  putStrLn $ "problem parsing JSON: " ++ body
  putStrLn $ show e

logChange :: [(String,String)] -> IO ()
logChange jsonData = do
  putStrLn $ "msg data: " ++ (show jsonData)

