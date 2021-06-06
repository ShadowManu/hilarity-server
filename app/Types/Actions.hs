{-# LANGUAGE OverloadedStrings #-}

module Types.Actions
(
) where

import Data.Aeson

import Types.Common

data Actions
  = SignIn UserId
  | SignOut UserId

-- instance FromJSON Person where
