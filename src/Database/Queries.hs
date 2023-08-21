{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}

module Database.Queries                 (addUser ,getUserByEmail ,getUserByKey 
                                        ,assignToken ,getSecret
                                        ,getMovie ,addMovieUnique, updMovie ,delMovie
                                        ,getFavMovies)
    where

import Database.Type                    as DbT
import Database.Persist
import Database.Persist.Postgresql
import Control.Monad.Reader
import qualified Database.Esqueleto     as Esq
import qualified Types                  as T

-- User :

addUser :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistUniqueWrite backend) => T.User -> ReaderT backend m (Maybe (Esq.Key DbT.User))
addUser user            = insertUnique $ User (T.name user) (T.email user) (T.password user)

getUserByEmail :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistUniqueRead backend) => String -> ReaderT backend m (Maybe (Entity DbT.User))
getUserByEmail email    = getBy $ UserPrimaryKey email

getUserByKey :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistUniqueRead backend) => Esq.Key DbT.User -> ReaderT backend m (Maybe (Entity DbT.User))
getUserByKey key        = getEntity key

-- User Authentication: 

assignToken :: BaseBackend backend ~ SqlBackend, MonadIO m, PersistQueryWrite backend) => String -> String -> ReaderT backend m (Maybe DbT.UserToken)
assignToken userEmail newToken = insertUnique $ UserToken userEmail newToken

getSecret :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistUniqueRead backend) => String -> ReaderT backend m (Maybe (Entity DbT.UserToken))
getSecret userEmail = getBy $ UserTokenPrimaryKey userEmail

-- Movie

-- checkMovieExists :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistUniqueRead backend) => String -> T.Movie ->  ReaderT backend m Bool
-- checkMovieExists userEmail movie = existsBy $ MoviePrimaryKey (T.movieName movie) userEmail

getMovie :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistUniqueRead backend) => String -> String -> ReaderT backend m (Maybe (Entity DbT.Movie))
getMovie userEmail movieName = getBy $ MoviePrimaryKey movieName userEmail

getFavMovies :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistQueryRead backend) => String -> ReaderT backend m [Entity DbT.Movie]
getFavMovies userEmail       = selectList [MovieCreatedBy ==. userEmail] []

addMovieUnique :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistUniqueWrite backend) => String -> T.Movie -> ReaderT backend m (Maybe ())
addMovieUnique userEmail movie    = insertUnique_ $ Movie (T.movieName movie) (T.rating movie) (T.genre movie) userEmail

updMovie :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistStoreWrite backend) => Esq.Key DbT.Movie -> String -> T.Movie -> ReaderT backend m ()
updMovie key userEmail movie = replace key $ Movie (T.movieName movie) (T.rating movie) (T.genre movie) userEmail

delMovie :: (BaseBackend backend ~ SqlBackend, MonadIO m, PersistStoreWrite backend) => Esq.Key DbT.Movie -> ReaderT backend m ()
delMovie key = delete key
