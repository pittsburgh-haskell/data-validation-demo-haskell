module OptionWinner where

import Data.Function ((&))

newtype Person = Person { getName :: String }
                 deriving (Show, Eq)

-- | Assume: bestFriend, oldestSister, youngestChild
-- each returns 'Maybe Person'
winner :: Person -> Maybe Person
winner person = do
  friend <- person & bestFriend
  sister <- friend & oldestSister
  sister & youngestChild

-- | Stub
bestFriend :: Person -> Maybe Person
bestFriend p | p == personA = Just $ Person "B"
             | otherwise = Nothing

-- | Stub
oldestSister :: Person -> Maybe Person
oldestSister p | p == personB = Just $ Person "C"
               | otherwise = Nothing

-- | Stub
youngestChild :: Person -> Maybe Person
youngestChild p | p == personY = Just $ Person "Z"
                | otherwise = Nothing

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
