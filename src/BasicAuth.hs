{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}
{-# LANGUAGE DataKinds             #-}

module BasicAuth                            (authCheck)
	where
import Data.Text                            (unpack)
import Servant.API.BasicAuth                (BasicAuthData (BasicAuthData))
import Servant.Server.Experimental.Auth     ()
import qualified Types                      as T
import Data.Text.Encoding                   (decodeUtf8)
import qualified Database.Type              as DbT
import qualified Database.Functions         as DbF
import Database.Persist                     (Entity(entityVal))
import Servant.Server                       (BasicAuthCheck (BasicAuthCheck)
                                            ,BasicAuthResult(Authorized, Unauthorized, NoSuchUser))

authCheck :: BasicAuthCheck T.UserForAuth
authCheck =
  let 
    check (BasicAuthData email secret) = do
      x <- DbF.getSecret $ unpack (decodeUtf8 email)
      case x of
        Just userToken -> if unpack (decodeUtf8 email) == DbT.userTokenUserEmail (entityVal userToken) && unpack (decodeUtf8 secret) == DbT.userTokenSecret (entityVal userToken)
          then return (Authorized (T.UserForAuth{T.emailAuth = DbT.userTokenUserEmail (entityVal userToken) }) )
          else return Unauthorized
        Nothing ->  return NoSuchUser
  in BasicAuthCheck check