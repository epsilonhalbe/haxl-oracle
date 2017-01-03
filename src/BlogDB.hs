{-# LANGUAGE
    DeriveGeneric, EmptyDataDecls, FlexibleContexts, FlexibleInstances, GADTs,
    GeneralizedNewtypeDeriving, MultiParamTypeClasses, OverloadedStrings, CPP,
    QuasiQuotes, TemplateHaskell, TypeFamilies
 #-}

module BlogDB where

import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Logger   (LoggingT, runStderrLoggingT)
import           Control.Monad.Reader   (ReaderT, forM)
import           Data.Text              (Text)
import           Data.Time.Clock        (UTCTime)
import           Database.Persist       (selectKeysList, selectList, (==.))
import           Database.Persist.ODBC  (ConnectionString, Entity, Key,
                                         SqlBackend, oracle, printMigration,
                                         runSqlPool, withODBCPool)
import           Database.Persist.TH    (mkDeleteCascade, mkMigrate, mkPersist,
                                         mpsGeneric, persistUpperCase, share,
                                         sqlSettings)
#ifdef MIGRATE
import           Database.Persist.ODBC   (runMigration)
#endif

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

getPostIds :: Blog [Key Post]
getPostContent :: Key Post -> Blog [Entity PostContent]
 -- select postid from postinfo;
getPostIds = selectKeysList [] []
 -- select * from postcontent where postid = pid;
getPostContent pid = selectList [PostContentPid ==. pid] []

main :: IO ()
main = runBlog $ do
#ifdef MIGRATE
  runMigration migrateAll
#else
  printMigration migrateAll
#endif
  xs <- getPostIds
  ys <- forM xs getPostContent
  liftIO $ print ys

runBlog :: Blog a -> IO a
runBlog = runStderrLoggingT . withODBCPool (Just oracle) conn 10 . runSqlPool
  where conn :: ConnectionString
        conn = "DSN=ORACLE11"

