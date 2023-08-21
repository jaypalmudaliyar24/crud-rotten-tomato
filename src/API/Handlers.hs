{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE DataKinds             #-}

module API.Handlers     
                                            (registerUser ,loginUser
                                            -- ,logoutUser
                                            ,getFavMovies ,addFavMovie ,updFavMovie ,delFavMovie)
    where

import Prelude                              ()
import Prelude.Compat
import Control.Monad.Except
import qualified Database.Functions         as DbF
import qualified Database.Type              as DbT
import Database.Esqueleto                   (Entity(entityVal, entityKey))
import Types                                (Movie(..), User, UserForAuth(..))
import Servant                              (Handler, NoContent, err400, err401, err404, err409, err500)
import qualified Servant.API.ContentTypes   as SACT             
import Data.Aeson                           (parseJSON)
import System.Random                        as Rnd
import Data.Char                            (chr)

tokenLength :: Int
tokenLength = 20

randomChar :: IO Char
randomChar = do
                randomInt <- Rnd.randomRIO (97, 122)
                return $ chr randomInt

randomString :: Int -> IO String
randomString n = sequence $ replicate n randomChar

registerUser :: User -> Handler DbT.User
registerUser user           = do
                                x <- liftIO $ DbF.addUser user
                                case x of
                                    Nothing             -> throwError err409
                                    Just createdUserKey -> do
                                                            maybeUser <- liftIO $ DbF.getUserByKey createdUserKey
                                                            case maybeUser of
                                                                Nothing     -> throwError err404
                                                                Just user   -> return (entityVal user)

loginUser :: Maybe String -> Maybe String -> Handler DbT.UserToken
loginUser email password    = case email of
                                Nothing -> throwError err400
                                Just emailx -> case password of
                                    Nothing -> throwError err400
                                    Just passwordx -> do
                                        x <- liftIO $ DbF.getUserByEmail emailx
                                        case x of
                                            Nothing -> throwError err400
                                            Just user -> if DbT.userPassword (entityVal user) /= passwordx
                                                then throwError err401
                                                else do
                                                        genToken <- liftIO $ randomString tokenLength
                                                        liftIO $ DbF.assignToken email genToken
                                                        liftIO $ DbF.getSecret email
                            

logoutUser :: User -> Handler User
logoutUser user = error "TODO : Implement 'addFavMovie'"

getFavMovies :: UserForAuth -> Handler [DbT.Movie]
getFavMovies userAuth    = do
                            let userEmail = emailAuth userAuth
                            movies <- liftIO $ DbF.getFavMovies userEmail
                            return (map entityVal movies)

addFavMovie :: UserForAuth -> Movie -> Handler ()
addFavMovie userAuth movie = do
                                let userEmail = emailAuth userAuth
                                res <- liftIO $ DbF.addMovieUnique userEmail movie
                                case res of
                                    Nothing -> throwError err400
                                    Just x -> return x
                                

updFavMovie :: UserForAuth -> Movie -> Handler ()
updFavMovie userAuth movie = do
                                let userEmail = emailAuth userAuth
                                res <- liftIO $ DbF.getMovie userEmail (movieName movie)
                                case res of
                                    Nothing -> throwError err400
                                    Just eMovie -> liftIO $ DbF.updMovie (entityKey eMovie) userEmail movie
                                                        
delFavMovie :: UserForAuth -> Maybe String -> Handler NoContent
delFavMovie userAuth qMovieName = case qMovieName of
                                    Nothing -> throwError err400
                                    Just movieName -> do
                                                        let userEmail = emailAuth userAuth
                                                        res <- liftIO $ DbF.getMovie userEmail movieName
                                                        case res of
                                                            Nothing -> throwError err400
                                                            Just eMovie -> do
                                                                                liftIO $ DbF.delMovie $ entityKey eMovie
                                                                                return SACT.NoContent