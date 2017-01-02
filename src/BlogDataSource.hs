{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE EmptyDataDecls             #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}


module BlogDataSource where
  {-( PostId, PostContent-}
  {-, getPostIds-}
  {-, getPostContent-}
  {-, initDataSource-}
  {-, BlogRequest(..)-}
  {-, BlogDBException(..)-}
  {-) where-}


import           Control.Exception
import           Control.Monad
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger   (LoggingT, runStderrLoggingT)
import           Control.Monad.Reader   (ReaderT, forM)
import           Data.Hashable
import           Data.List
import qualified Data.Map               as Map
import           Data.Maybe
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Data.Typeable
import           Database.Persist       (selectKeysList, selectList, (==.))
import           Database.Persist.ODBC  (ConnectionString, Entity, Key,
                                         SqlBackend, oracle, runSqlPool,
                                         createODBCPool, ConnectionPool)
import           Database.Persist.TH    (mkDeleteCascade, mkMigrate, mkPersist,
                                         mpsGeneric, persistUpperCase, share,
                                         sqlSettings)
import           Haxl.Core
import Database.SQLite


-- -----------------------------------------------------------------------------
-- Types

share [ mkPersist sqlSettings {mpsGeneric = False}
      , mkMigrate "migrateAll"
      , mkDeleteCascade sqlSettings {mpsGeneric = False}]
      [persistUpperCase|

Post            sql=POSTINFO
  Id    Int     sql=POSTID
  date  UTCTime sql=POSTDATE
  topic Text    sql=POSTTOPIC

  deriving Show Eq

PostContent      sql=POSTCONTENT
  pid     PostId sql=POSTID
  content Text   sql=CONTENT

  deriving Show Eq
  Primary pid

PostViews      sql=POSTVIEWS
  pvid  PostId sql=POSTID
  views Int    sql=VIEWS

  deriving Show Eq
  Primary pvid
|]

type Blog a = ReaderT SqlBackend (LoggingT IO) a

data BlogRequest a where
  FetchPosts       :: BlogRequest [Key Post]
  FetchPostContent :: PostId -> BlogRequest (Entity PostContent)

deriving instance Show (BlogRequest a)
deriving instance Typeable BlogRequest

instance Show1 BlogRequest where show1 = show

deriving instance Eq (BlogRequest a)

instance Hashable (BlogRequest a) where
  hashWithSalt salt FetchPosts = hashWithSalt salt (0::Int)
  hashWithSalt salt (FetchPostContent p) = hashWithSalt salt (1::Int, unPostKey p)


getPostIds :: GenHaxl u [Key Post]
getPostIds = dataFetch FetchPosts

getPostContent :: PostId -> GenHaxl u (Entity PostContent)
getPostContent = dataFetch . FetchPostContent

initDataSource :: IO (State BlogRequest)
initDataSource = BlogDataState <$> runStderrLoggingT (createODBCPool (Just oracle) conn 10)
  where conn = "DSN=ORACLE11"

instance StateKey BlogRequest where
  data State BlogRequest = BlogDataState ConnectionPool

instance DataSourceName BlogRequest where
  dataSourceName _ = "[Oracle 11g] BlogDataSource"

instance DataSource u BlogRequest where
  fetch (BlogDataState db) _flags _userEnv blockedFetches =
    SyncFetch $ batchFetch db blockedFetches

type Batches
  = ( [ResultVar [Key Post]]                       -- FetchPosts
    , [(Key Post, ResultVar (Entity PostContent))] -- FetchPostContent
    )

emptyBatches :: Batches
emptyBatches = ([],[])

collect :: BlockedFetch BlogRequest -> Batches -> Batches
collect (BlockedFetch FetchPosts v) (as,bs) = (v:as,bs)
collect (BlockedFetch (FetchPostContent x) v) (as,bs) = (as,(x,v):bs)

batchFetch :: ConnectionPool -> [BlockedFetch BlogRequest] -> IO ()
batchFetch = undefined


doFetch :: ConnectionPool -> Batches -> IO ()
doFetch db (as,bs) = undefined

sqlMultiFetch
  :: ConnectionPool           -- db
  -> [x]                      -- requests
  -> (x -> ResultVar a)       -- getvar
  -> String                   -- query
  -> ([y] -> z)               -- collate
  -> (x -> z -> Maybe a)      -- extract
  -> IO ()

sqlMultiFetch db [] =
  do x <- runSqlPool (selectKeysList [] [] :: Blog [Key Post]) db
     return x













