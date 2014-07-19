module Main where
import Text.HTML.Download
import Directory
import Text.Regex.Posix
-- import Data.List
import System.Posix
import IO

-- for MySQL
import Control.Monad
import Database.HDBC
import Database.HDBC.MySQL

-- CONFIG --
listingPagesDir = "./NewListing/"
listPageBaseUrl = "http://www.charitynavigator.org/index.cfm?bay=search.alpha&ltr="
orgPagesDir = "./Org/"
orgPageBaseUrl = "http://www.charitynavigator.org/index.cfm?bay=search.summary&orgid="

orgIdRe = "<a href=\"http://www\\.charitynavigator\\.org/index\\.cfm\\?bay=search\\.summary\\&amp;orgid=([0-9]*)\">"
charityNameRe = "<h1 class=\"charityname\">(.*?)<\\/h1>"
{- <tr>
     <td><strong>Total Revenue</strong></td>
     <td align="right"><strong>$2,447,252</strong></td>    -- can be negative!
   </tr> -}
revenueRe = "<tr>\\s*<td>\\s*<strong>Total Revenue<\\/strong>\\s*<\\/td>\\s*<td align=\"right\"><strong>(\\$-?[0-9,]*)<\\/strong><\\/td>\\s*<\\/tr>"

main :: IO ()
main = do
  -- fetchListingPages
  ids <- orgIds  -- fmap: applies regular (non-monadic) fun over monadic val
  -- fetchOrgPages ids
  mapM_ printIdNameAndRevenue ids

-- fetching --
fetchListingPages :: IO ()
fetchListingPages =
    mapM_ fetch firstChars  -- mapM: maps monadic fun over (regular) list
    where fetch ch = savePageToFile (listPageBaseUrl ++ [ch]) $ listingPagesDir ++ (ch : ".html")
          firstChars = ['A'..'Z'] ++ ['1','9']

fetchOrgPages :: [Int] -> IO ()
fetchOrgPages ids =
    mapM_ fetch ids
    where fetch id = savePageToFile (orgPageBaseUrl ++ show id) $ orgPagesDir ++ show id ++ ".html"

savePageToFile :: String -> FilePath -> IO ()
savePageToFile url fileName =
    -- FIXME.  Do only if file doesn't already exist.
    do src <- (sleep 5 >> openURL url)
       writeFile fileName src

htmlFilesInDir :: FilePath -> IO [FilePath]
htmlFilesInDir dir =
    fmap (filter isHtml) $ getDirectoryContents dir
    where isHtml s = s =~ "\\.html$"

-- parsing --
orgIds :: IO [Int]
orgIds =
    do paths <- htmlFilesInDir listingPagesDir
       ids <- mapM orgIdsFromListingPage paths
       return $ concat ids

orgIdsFromListingPage :: FilePath -> IO [Int]
orgIdsFromListingPage fileName =
    do fh <- openFile (listingPagesDir ++ fileName) ReadMode
       str <- hGetContents fh
       return $ orgIdsFromStr str

orgIdsFromStr :: String -> [Int]
orgIdsFromStr str = aux str []
    where aux str' acc =
              -- (before match, of match, after match, [matchGroup])
              case str' =~ orgIdRe :: (String,String,String,[String]) of
                (_,"","",_)         -> acc
                (_,_,after,[idStr]) -> aux after ((read idStr) : acc)

printIdNameAndRevenue :: Int -> IO ()
printIdNameAndRevenue id =
    do fh <- openFile (orgPagesDir ++ show id ++ ".html") ReadMode
       str <- hGetContents fh
       let name = getCharityName str
           rev = getTotalRevenue str
       putStr $ (show id) ++ "\t" ++ name ++ "\t" ++ (show rev) ++ "\n"
       hClose fh

getTotalRevenue :: String -> Int
getTotalRevenue html = read digits
    where (_,_,_,[str]) = html =~ revenueRe :: (String,String,String,[String])
          digits = filter (\c -> c `elem` ['0'..'9']) str

getCharityName :: String -> String
getCharityName html = str
    where (_,_,_,[str]) = html =~ charityNameRe :: (String,String,String,[String])


{-
  MySQL users are encouraged to use the ODBC driver, which works and has been
  tested against MySQL on both Linux/Unix and Windows platforms.
  But HDBC-odbc ('cabal install' of same) failed, due to missing C lib.

  HDBC:  http://software.complete.org/static/hdbc/doc/Database-HDBC.html
  MySQL: http://hackage.haskell.org/packages/archive/HDBC-mysql/0.6/doc/html/Database-HDBC-MySQL.html
-}
dbStuff :: IO ()
dbStuff =
    do conn <- connectMySQL defaultMySQLConnectInfo {
                 mysqlHost     = "dbhost"
               , mysqlUser     = "root"
               , mysqlPassword = ""
               }
       -- rows <- quickQuery' conn "SELECT 1 + 1" []
       rows <- quickQuery conn "SELECT 1 + 1" []
       forM_ rows $ \row -> putStrLn $ show row
