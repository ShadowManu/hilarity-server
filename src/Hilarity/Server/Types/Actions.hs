{-# LANGUAGE OverloadedStrings #-}

module Hilarity.Server.Types.Actions
(
) where

import Data.Aeson

import Hilarity.Server.Types.Common

data Actions
  = SignIn UserId
  | SignOut UserId

-- instance FromJSON Person where
