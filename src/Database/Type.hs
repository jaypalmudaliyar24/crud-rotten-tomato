{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}


module Database.Type                where

import Database.Persist
import Database.Persist.Postgresql
import Database.Persist.TH
import Data.Time                    (Day (ModifiedJulianDay))
import qualified Types              as T

import Data.Aeson                   (FromJSON, ToJSON(toJSON), parseJSON, (.:), (.:?), (.=), object, withObject)
import GHC.Generics                 (Generic)

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
User sql = users
    name String
    email String
    password String
    Primary email
    deriving Show Read Generic

UserToken sql = user_tokens
    userEmail String
    secret String
    Primary userEmail
    deriving Show Read Generic

Movie sql = movies
    name String
    rating Double Maybe
    genre String Maybe
    createdBy String
    Primary name createdBy
    deriving Show Read Generic
|]

instance ToJSON User 
    where
        toJSON (User name email password) = object $
            (["name" .= name] ++
            ["email" .= email] ++
            ["password" .= password])

instance FromJSON User 
    where
        parseJSON = withObject "User" $ \obj ->
            User
                <$> obj .: "name"
                <*> obj .: "email"
                <*> obj .: "password"

instance ToJSON Movie 
    where
        toJSON (Movie movieName rating genre createdBy) = object $
            (["movieName" .= movieName] ++
            ["rating" .= rating] ++
            ["genre" .= genre] ++ 
            ["createdBy" .= createdBy])

instance FromJSON Movie 
    where
        parseJSON = withObject "Movie" $ \obj ->
            Movie   
                <$> obj .: "movieName"
                <*> obj .: "rating"
                <*> obj .: "genre"
                <*> obj .: "createdBy"

instance ToJSON UserToken
    where
        toJSON (UserToken userEmail secret) = object $
            (["userEmail" .= userEmail] ++
            ["secret" .= secret])

instance FromJSON UserToken
    where
        parseJSON = withObject "UserToken" $ \obj ->
            User
                <$> obj .: "userEmail"
                <*> obj .: "secret"