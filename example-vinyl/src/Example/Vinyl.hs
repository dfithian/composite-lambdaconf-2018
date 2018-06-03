module Example.Vinyl where

import ClassyPrelude
import Control.Lens
import Data.Proxy
import Data.Vinyl

-- lens

type X = '[ Char, Int ]

x :: Rec Identity X
x = Identity 'a' :& Identity 1 :& RNil

extractInt :: (Int ∈ rs) => Rec f rs -> f Int
extractInt = view (rlens Proxy)

-- subset

type A = '[ Char ]

projectA :: (A ⊆ rs) => Rec f rs -> Rec f A
projectA = view rsubset

-- equivalence

type X' = '[ Int, Char ]

reorder :: (a ≅ b) => Rec f a -> Rec f b
reorder = view rsubset

-- traverse

justInside :: Functor f => f x -> f (Maybe x)
justInside = map Just

justOutside :: Functor f => f x -> Maybe (f x)
justOutside = Just

-- user example

newtype Username = Username Text deriving Show
newtype Password = Password Text deriving Show
newtype Email = Email Text deriving Show

type User = '[Username, Password]
type RegisteredUser = '[Email, Username, Password]

registerUser :: Email -> Rec Identity User -> Rec Identity RegisteredUser
registerUser email user = Identity email :& user

unregisteredUser :: Rec Identity User
unregisteredUser = Identity (Username "dan") :& Identity (Password "*****") :& RNil

projectUser :: (User ⊆ rs) => Rec f rs -> Rec f User
projectUser = view rsubset

registeredUser :: Rec Identity RegisteredUser
registeredUser = Identity (Email "dan@dan.dan") :& Identity (Username "dan") :& Identity (Password "*****") :& RNil

verifyField :: Maybe x -> Maybe (Identity x)
verifyField = map Identity

verifyUser :: Rec Maybe User -> Maybe (Rec Identity User)
verifyUser = rtraverse verifyField

goodUser, badUser :: Rec Maybe User
goodUser = Just (Username "dan") :& Just (Password "*****") :& RNil
badUser = Nothing :& Just (Password "*****") :& RNil
