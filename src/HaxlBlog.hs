{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module HaxlBlog (getPostIds, getPostContent, Haxl, run) where


import           BlogDataSource
import           Haxl.Core

type Haxl a = GenHaxl () a

run :: Haxl a -> IO a
run h = do
  db <- initDataSource
  env <- initEnv (stateSet db stateEmpty) ()
  runHaxl env h

