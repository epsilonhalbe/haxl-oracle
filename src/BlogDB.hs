{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}

module BlogDB where

{-import Database.Persist-}
{-import Database.Persist.ODBC-}
{-import Control.Monad.Logger-}

{-import Control.Exception-}
{-import Data.Typeable-}
{-import Control.Monad.IO.Class-}
{-import Control.Monad.Trans.Reader-}

import           Control.Monad.IO.Class  (liftIO)
import           Control.Monad.Logger
import           Database.Persist
import           Control.Monad.Reader
import           Data.Text
import           Data.Time.Clock
import           Database.Persist.Sql
import           Database.Persist.ODBC
import           Database.Persist.TH
import           Data.Typeable

share [ mkPersist sqlSettings {mpsGeneric = False} , mkMigrate "compositeMigrate" , mkDeleteCascade sqlSettings {mpsGeneric = False}] [persistUpperCase|

Post sql=POSTINFO
  Id Int sql=POSTID
  date UTCTime sql=POSTDATE
  topic Text sql=POSTTOPIC

  deriving Show Eq

PostContent
  Id Int sql=POSTID
  content Text sql=CONTENT
  deriving Show Eq

PostViews
  Id PostId sql=POSTID
  views Int sql=VIEWS
  deriving Show Eq
|]

conn :: ConnectionString
conn = "DSN=ORACLE11"

getPostIds :: MonadIO m => ReaderT SqlBackend m [Entity Post]
getPostIds = rawSql "select ?? from postinfo" []

main :: IO ()
main = runDB $ do
  {-runMigration migrateAll-}
  xs <- selectList [] []
  liftIO $ print (xs :: [Entity Post])

runDB :: ReaderT SqlBackend (LoggingT IO) a -> IO a
runDB = runStderrLoggingT . withODBCConn (Just oracle) conn . runSqlConn
-- -----------------------------------------------------------------------------
-- A monad


{-share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|-}
{-Post-}
    {-content String-}
    {-deriving Show-}
{-|]-}


{-type Blog a = SqlBackend -> NoLoggerT IO a-}

{-run :: Blog a -> IO a-}
{-run query = runNoLoggerT $ withODBCConn (Just oracle)-}
                           {-"system/oracle@//localhost:14161" query-}


{--- ------------------------------------------------------------------------------}
{--- An API-}

{-type PostId = Int-}
{-type PostContent = String-}

{-getPostIds     :: Blog [PostId]-}
{-getPostContent :: PostId -> Blog PostContent-}
-- -- more operations...
--
--
-- -- -----------------------------------------------------------------------------
-- -- Implementation
--
-- sql :: String -> Blog (Either String [[Row Value]])
-- sql query = do
--   db <- ask
--   liftIO $ do
--     putStrLn query
--     execStatement db query
--
--
{-getPostIds = do undefined-}
--   r <- sql "select postid from postinfo;"
--   case r of
--     Right [rows] -> return [ fromIntegral id | [(_,Int id)] <- rows ]
--     Left s -> liftIO $ throwIO (BlogDBException s)
--     _ -> liftIO $ throwIO (BlogDBException "invalid result")
--
{-getPostContent x = do undefined-}
--   r <- sql ("select content from postcontent where postid = " ++
--             show x ++ ";")
--   case r of
--     Right [[[(_,Text str)]]] -> return str
--     Left s -> liftIO $ throwIO (BlogDBException s)
--     _ -> liftIO $ throwIO (BlogDBException "invalid result")
--
-- data BlogDBException = BlogDBException String
--   deriving (Show, Typeable)
--
-- instance Exception BlogDBException
