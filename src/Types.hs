{-# LANGUAGE DeriveGeneric  #-}
{-# LANGUAGE InstanceSigs   #-}
{-# LANGUAGE DeriveAnyClass #-}

module Types 
                            (Movie(..)
                            ,User(..)
                            ,UserForAuth(..)) 
    where

import GHC.Generics         (Generic)
import Data.Aeson           (ToJSON, FromJSON, toEncoding, genericToEncoding, defaultOptions)
import Data.Time            (Day (ModifiedJulianDay))
import Servant              (FromHttpApiData (parseQueryParam))
import Data.Text            (Text, unpack)
import qualified Data.Maybe as DM

data Movie              = Movie
                        {
                            movieName :: String
                            ,rating :: DM.Maybe Double
                            ,genre :: DM.Maybe String
                        } deriving (Show, Generic)
instance                ToJSON Movie 
    where
        toEncoding = genericToEncoding defaultOptions
instance                FromJSON Movie

data User               = User 
                        {
                            name :: String
                            ,email :: String
                            ,password :: String
                        } deriving (Show, Generic)
instance                ToJSON User
    where
        toEncoding = genericToEncoding defaultOptions
instance                FromJSON User

newtype UserForAuth     = UserForAuth
                        {
                            emailAuth :: String
                        } deriving (Show, Eq)
