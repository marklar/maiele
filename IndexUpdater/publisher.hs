module Main where

import Network.AMQP
import qualified Data.ByteString.Lazy.Char8 as BL
import Control.Monad (forever)

-- config --
connIP = "127.0.0.1"
vHost = "/"
user = "guest"
pwd = "guest"
xName = "mark.glu_stock"
xType = "fanout"
routingKey = ""

main = do
    conn <- openConnection connIP vHost user pwd
    chan <- openChannel conn
    -- exchange
    declareExchange chan newExchange { exchangeName = xName
                                     , exchangeType = xType
                                     }
    pub chan

pub :: Channel -> IO ()  
pub chan =
    forever $ do
      ln <- getLine

      -- Msg w/ null storefront_id -- to be used.
      let jsonStr = "{\"glu_id\":1234, \"is_in_stock\":true, \"storefront_id\":null}"
      publishMsg chan xName routingKey 
              newMsg { msgBody = BL.pack jsonStr
                     , msgDeliveryMode = Just Persistent
                     }

      -- Msg to be ignored.
      let jsonStr = "{\"glu_id\":1234, \"is_in_stock\":true, \"storefront_id\":1234}"
      publishMsg chan xName routingKey 
              newMsg { msgBody = BL.pack jsonStr
                     , msgDeliveryMode = Just Persistent
                     }
