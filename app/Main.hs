module Main                     (main) where

import Network.Wai.Handler.Warp
import API.Endpoints            (basicAuthApi, server)
import BasicAuth                (authCheck)
import Servant                  (serveWithContext, Context ((:.), EmptyContext))
import Database.Functions       as DbF

main :: IO ()
main = do
    print "Server Started"
    x <- DbF.migrationScript
    print "Ran Migrations " ++ show x
    run 8081 (serveWithContext basicAuthApi (authCheck :. EmptyContext) server)
