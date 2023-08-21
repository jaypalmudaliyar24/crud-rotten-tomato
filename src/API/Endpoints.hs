{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE ScopedTypeVariables   #-}

module API.Endpoints
                                (UserRegister
                                ,UserLogin
                                -- ,UserLogout
                                ,AddFavMovie
                                ,basicAuthApi
                                ,server)
    where


import API.Handlers             (registerUser ,loginUser 
                                -- ,logoutUser 
                                ,getFavMovies ,addFavMovie ,updFavMovie ,delFavMovie)
import Prelude                  ()
import Prelude.Compat
import Servant
import Types                    (Movie, User, UserForAuth)
import qualified Database.Type  as DbT

-- User

type UserRegister       = "register"    :> ReqBody '[JSON] User :> PostCreated '[JSON] DbT.User

type UserLogin          = "login"       :> QueryParam "email" String 
                                        :> QueryParam "password" String :> Get '[JSON] DbT.UserToken

-- type UserLogout         = BasicAuth "user-realm" User :> "logout" :> Get '[JSON] DbT.User

type GetFavMovie        = BasicAuth "user-realm" UserForAuth :>
                        "getFavMovie"   :> Get '[JSON] [DbT.Movie]
type AddFavMovie        = BasicAuth "user-realm" UserForAuth :> 
                        "addFavMovie"   :> ReqBody '[JSON] Movie :> PostCreated '[JSON] ()
type UpdateFavMovie     = BasicAuth "user-realm" UserForAuth :>
                        "updateFavMovie":> ReqBody '[JSON] Movie :> Put '[JSON] ()
type DeleteFavMovie     = BasicAuth "user-realm" UserForAuth :> 
                        "deleteFavMovie":> QueryParam "movieName" String :> DeleteNoContent

type UserApi            = UserRegister 
                        :<|> UserLogin
                        -- :<|> UserLogout 
                        :<|> GetFavMovie
                        :<|> AddFavMovie
                        :<|> UpdateFavMovie
                        :<|> DeleteFavMovie
                        

type CombinedAPI        = "user"        :> UserApi

userServer              :: Server UserApi
userServer              = registerUser 
                        :<|> loginUser 
                        -- :<|> logoutUser
                        :<|> getFavMovies
                        :<|> addFavMovie
                        :<|> updFavMovie
                        :<|> delFavMovie

server                  :: Server CombinedAPI
server                  = userServer

basicAuthApi            :: Proxy CombinedAPI
basicAuthApi            = Proxy

