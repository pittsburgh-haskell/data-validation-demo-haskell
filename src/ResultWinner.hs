module ResultWinner where

import Data.Function ((&))

newtype Person = Person { getName :: String }
                 deriving (Show, Eq)

-- | For illustration only. String errors are terrible practice.
newtype MyError = MyError { getMessage :: String }
                  deriving (Show, Eq)

-- | Assume: bestFriend, oldestSister, youngestChild
-- each returns 'Either MyError Person'
winner :: Person -> Either MyError Person
winner person = do
  friend <- person & bestFriend
  sister <- friend & oldestSister
  sister & youngestChild

-- | Stub
bestFriend :: Person -> Either MyError Person
bestFriend p | p == personA = Right $ Person "B"
             | otherwise =
               Left $ MyError $ getName p ++ " has no best friend"

-- | Stub
oldestSister :: Person -> Either MyError Person
oldestSister p | p == personB = Right $ Person "C"
               | otherwise =
                 Left $ MyError $ getName p ++ " has no oldest sister"

-- | Stub
youngestChild :: Person -> Either MyError Person
youngestChild p | p == personY = Right $ Person "Z"
                | otherwise =
                  Left $ MyError $ getName p ++ " has no youngest child"

personA :: Person
personA = Person "A"

personB :: Person
personB = Person "B"

personC :: Person
personC = Person "C"

personY :: Person
personY = Person "Y"

personZ :: Person
personZ = Person "Z"
