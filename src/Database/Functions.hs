module Database.Functions           (addUser ,getUserByEmail ,getUserByKey 
                                    ,assignToken ,getSecret ,removeToken
                                    ,getMovie ,addMovieUnique ,updMovie ,delMovie
                                    ,getFavMovies
                                    ,migrationScript)
    where

import Database.Type                as DbT
import qualified Database.Queries   as DbQ

import Database.Persist
import Database.Esqueleto.Legacy    as Esq
import Database.Persist.Postgresql
import Control.Monad.Logger
import qualified Types              as T
import Data.ByteString              (ByteString)
import Data.ByteString.Char8        as BS

connectionString :: ByteString
connectionString                = BS.pack "host=postgres dbname=postgres user=postgres password=postgres port=5432"

migrationScript :: IO ()
migrationScript                 = runAction $ runMigration migrateAll

runAction :: SqlPersistT (NoLoggingT IO) a -> IO a
runAction action                = runNoLoggingT $ withPostgresqlConn connectionString (runSqlConn action)

-- User

addUser :: T.User -> IO (Maybe (Esq.Key DbT.User))
addUser user                    = runAction $ DbQ.addUser user

getUserByKey :: Esq.Key DbT.User -> IO (Maybe (Entity DbT.User))
getUserByKey userKey            = runAction $ DbQ.getUserByKey userKey

getUserByEmail :: String -> IO (Maybe (Entity DbT.User))
getUserByEmail email            = runAction $ DbQ.getUserByEmail email

-- User Authentication

assignToken :: String -> String -> IO (Maybe ())
assignToken userEmail newToken  = runAction $ DbQ.assignToken userEmail newToken

getSecret :: String -> IO (Maybe (Entity DbT.UserToken))
getSecret userEmail             = runAction $ DbQ.getSecret userEmail

removeToken :: Esq.Key DbT.UserToken -> IO ()
removeToken key                 = runAction $ DbQ.removeToken key

-- Movie

getMovie :: String -> String -> IO (Maybe (Entity DbT.Movie))
getMovie userEmail movieName    = runAction $ DbQ.getMovie userEmail movieName

getFavMovies :: String -> IO [Entity DbT.Movie]
getFavMovies userEmail          = runAction $ DbQ.getFavMovies userEmail

addMovieUnique :: String -> T.Movie -> IO (Maybe ())
addMovieUnique userEmail movie  = runAction $ DbQ.addMovieUnique userEmail movie

updMovie :: Esq.Key DbT.Movie -> String -> T.Movie -> IO ()
updMovie key userEmail movie    = runAction $ DbQ.updMovie key userEmail movie

delMovie :: Esq.Key DbT.Movie -> IO ()
delMovie key                    = runAction $ DbQ.delMovie key
