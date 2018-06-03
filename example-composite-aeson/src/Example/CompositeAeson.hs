module Example.CompositeAeson where

import ClassyPrelude
import Composite
import Composite.Aeson
import qualified Data.Aeson.BetterErrors as ABE
import qualified Data.ByteString.Lazy as L

newtype Username = Username Text deriving Show
newtype Password = Password Text deriving Show
newtype Email = Email Text deriving Show

type RegisteredUser = '[ "user" :-> Username
                       , "password" :-> Password
                       , "email" :-> Email
                       ]

objectKeyAsText :: FromField Text Text
objectKeyAsText = FromField $ \ x -> ABE.key x ABE.asText

registeredUserRecordFromJson :: Rec (FromField Text) RegisteredUser
registeredUserRecordFromJson =
  (Username <$> objectKeyAsText)
    :^: (Password <$> objectKeyAsText)
    :^: (Email <$> objectKeyAsText)
    :^: RNil

fromJson :: ABE.Parse Text (Record RegisteredUser)
fromJson = recordFromJson registeredUserRecordFromJson

good, missingEmail, badUsername :: L.ByteString
good = "{\"user\": \"dan\", \"password\": \"*****\", \"email\": \"dan@dan.dan\"}"
missingEmail = "{\"user\": \"dan\", \"password\": \"*****\"}"
badUsername = "{\"user\": 1, \"password\": \"*****\", \"email\": \"dan@dan.dan\"}"
