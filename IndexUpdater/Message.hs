module Message where

import Char (toUpper)
import Data.List (find)
import Maybe (fromJust)

data Msg = Msg Int Bool (Maybe Int)

-- [("glu_id","1234"),("is_in_stock","true"),("storefront_id","null")]
getMsgData :: [(String,String)] -> Msg
getMsgData alist =
    Msg (gluId alist) (isInStock alist) (storeId alist)

gluId :: [(String,String)] -> Int
gluId alist =
    read str :: Int
    where str = findVal "glu_id" alist

isInStock :: [(String,String)] -> Bool
isInStock alist =
    read $ capitalize str :: Bool
    where str = findVal "is_in_stock" alist
          capitalize (x:xs) = toUpper x : xs

storeId :: [(String,String)] -> Maybe Int
storeId alist =
    if str == "null"
      then Nothing
      else Just $ (read str :: Int)
    where str = findVal "storefront_id" alist

findVal :: Eq a => a -> [(a,b)] -> b
findVal key =
    snd . fromJust . find (\(k,_) -> k == key)
