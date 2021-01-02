module Day4 where

import Util
import Data.List
import Data.Bifunctor

data Field = Valid String | Invalid String | Unknown String String deriving Show

tagUnvalidated :: (String, String) -> Field
tagUnvalidated (k, v) = Unknown k v

readData :: String -> [[(String,String)]]
readData = map concat . splitOn [] . map (map (second tail . break (== ':')) . words) . lines

fields = ["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid", "cid"] 

allPresent = null           -- No compulsory fields unaccounted for
           . (\\ ["cid"])   -- cid is not mandatory
           . (fields \\)    -- Remove all present fields from required fields
           . map fst        -- Only care about keys for now

validator :: String -> (String -> Bool) -> Field -> Field
validator s p (Unknown k v) | s == k && p v = Valid k      -- Key matches and value admissible
                            | s == k        = Invalid k    -- Value not admissible for key
                            | otherwise     = Unknown k v  -- This is not the validator we're looking for
validator _ _ f = f

validByr, validIyr, validEyr, validHgt, validHcl, validEcl, validPid, validCid :: Field -> Field

--  byr (Birth Year) - four digits; at least 1920 and at most 2002.
validByr = validator "byr" (within (1920, 2002) . read)

--  iyr (Issue Year) - four digits; at least 2010 and at most 2020.
validIyr = validator "iyr" (within (2010, 2020) . read)

--  eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
validEyr = validator "eyr" (within (2020, 2030) . read)

--  hgt (Height) - a number followed by either cm or in:
--      If cm, the number must be at least 150 and at most 193.
--      If in, the number must be at least 59 and at most 76.
validHgt = validator "hgt" $ \x -> case break (not . digit) x of
                                     (cm, "cm") -> within (150, 193) $ read cm
                                     (i, "in") -> within (59, 76) $ read i
                                     _ -> False

--  hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
validHcl = validator "hcl" $ \x -> case uncons x of 
                                     Just ('#', code) -> all hex code && length code == 6
                                     _ -> False

--  ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
validEcl = validator "ecl" $ flip elem ["amb", "blu", "brn", "gry", "grn", "hzl", "oth"]

--  pid (Passport ID) - a nine-digit number, including leading zeroes.
validPid = validator "pid" $ \x -> all digit x && length x == 9

--  cid (Country ID) - ignored, missing or not.
validCid = validator "cid" $ const True

validate :: [(String, String)] -> Bool
validate pass = all isValid pass && allPresent pass
  where tryAllValidators :: Field -> Field
        tryAllValidators = compose [validByr, validIyr, validEyr, validHgt, validHcl, validEcl, validPid, validCid]
        isValid :: (String, String) -> Bool
        isValid pass = case tryAllValidators . tagUnvalidated $ pass of
                              (Valid _) -> True
                              _       -> False

challenge1 :: [[(String, String)]] -> Int
challenge1 = countTrue allPresent

challenge2 :: [[(String, String)]] -> Int
challenge2 = countTrue validate

main = do
  putStr "Sample (first) =  "
  result $ challenge1 . readData <$> readFile "data/sample4"
  putStr "Input (first) =  "
  result $ challenge1 . readData <$> readFile "data/input4"
  putStr "Sample (second) =  "
  result $ challenge2 . readData <$> readFile "data/sample4"
  putStr "Should all be invalid (0) =  "
  result $ challenge2 . readData <$> readFile "data/invalid4"
  putStr "Should all be valid (4) =  "
  result $ challenge2 . readData <$> readFile "data/valid4"
  putStr "Input (second) =  "
  result $ challenge2 . readData <$> readFile "data/input4"
