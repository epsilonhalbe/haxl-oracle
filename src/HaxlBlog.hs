{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TypeFamilies          #-}

module HaxlBlog (getPostIds, getPostContent, Haxl, run) where


import BlogDataSource
import Haxl.Core      (GenHaxl, initEnv, runHaxl, stateEmpty, stateSet)

type Haxl a = GenHaxl () a

run :: Haxl a -> IO a
run h = do
  db <- initDataSource
  env <- initEnv (stateSet db stateEmpty) ()
  runHaxl env h

