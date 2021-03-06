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

module BlogDataSource
  ( PostId
  , PostContent
  , getPostIds
  , getPostContent
  , initDataSource
  , BlogRequest(..)
  , BlogDBException(..)
  ) where


import           Control.Exception
import           Control.Monad
import           Control.Monad.Logger  (LoggingT, runStderrLoggingT)
import           Control.Monad.Reader  (ReaderT)
import           Data.Hashable
import           Data.List
import qualified Data.Map              as Map
import           Data.Text             (Text)
import           Data.Time.Clock       (UTCTime)
import           Data.Typeable
import           Database.Persist      (selectKeysList, selectList, (<-.))
import           Database.Persist.ODBC (ConnectionPool, Entity (..), Key,
                                        SqlBackend, createODBCPool, oracle,
                                        runSqlPool)
import           Database.Persist.TH   (mkDeleteCascade, mkMigrate, mkPersist,
                                        mpsGeneric, persistUpperCase, share,
                                        sqlSettings)
import           Haxl.Core


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
  FetchPostContent :: PostId -> BlogRequest PostContent

deriving instance Show (BlogRequest a)
deriving instance Typeable BlogRequest

instance Show1 BlogRequest where show1 = show

deriving instance Eq (BlogRequest a)

instance Hashable (BlogRequest a) where
  hashWithSalt salt FetchPosts = hashWithSalt salt (0::Int)
  hashWithSalt salt (FetchPostContent p) = hashWithSalt salt (1::Int, unPostKey p)


getPostIds :: GenHaxl u [Key Post]
getPostIds = dataFetch FetchPosts

getPostContent :: PostId -> GenHaxl u PostContent
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
    , [(Key Post, ResultVar PostContent)]          -- FetchPostContent
    )

emptyBatches :: Batches
emptyBatches = ([],[])

collect :: BlockedFetch BlogRequest -> Batches -> Batches
collect (BlockedFetch FetchPosts v) (as,bs)           = (v:as,bs)
collect (BlockedFetch (FetchPostContent x) v) (as,bs) = (as,(x,v):bs)

batchFetch :: ConnectionPool -> [BlockedFetch BlogRequest] -> IO ()
batchFetch db = doFetch db . foldr collect emptyBatches

doFetch :: ConnectionPool -> Batches -> IO ()
doFetch db (as,bs) = do
  sqlMultiFetch db as id
    (selectKeysList [] [] :: Blog [Key Post])
    id
    (\_ ids -> Just ids)

  sqlMultiFetch db bs snd
    (selectList [PostContentPid <-. map fst bs] [] :: Blog [Entity PostContent])
    (foldl' (\m x -> Map.insert (postContentPid $ entityVal x) (entityVal x) m) Map.empty)
    (\(x,_) y -> Map.lookup x y)

sqlMultiFetch
  :: ConnectionPool           -- db
  -> [x]                      -- requests
  -> (x -> ResultVar a)       -- getvar
  -> Blog [y]                 -- query
  -> ([y] -> z)               -- collate
  -> (x -> z -> Maybe a)      -- extract
  -> IO ()

sqlMultiFetch _ [] _ _ _ _ = return ()
sqlMultiFetch db requests getvar query collate extract =
  do result <- runStderrLoggingT $ runSqlPool query db
     let fetched = collate result
     forM_ requests $ \q ->
       case extract q fetched of
          Nothing -> putFailure (getvar q) (BlogDBException "missing result")
          Just r  -> putSuccess (getvar q) r

data BlogDBException = BlogDBException String
  deriving (Show, Typeable)

instance Exception BlogDBException where
  toException = transientErrorToException
  fromException = transientErrorFromException




