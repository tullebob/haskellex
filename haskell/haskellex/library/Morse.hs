module Morse (decode) where
import qualified Data.Map as Map

codes = [".-","-...","-.-.","-..",".","..-.","--.","....","..",".---","-.-",".-..",
         "--","-.","---",".--.","--.-",".-.","...","-","..-","...-",".--","-..-",
         "-.--","--.."]

encodings :: Map.Map Char String
encodings = Map.fromList (zip ['A'..'Z'] codes)

encode :: String -> String
encode [] = []
encode (x:xs) = case (Map.lookup x encodings) of -- alternatively: concatMap
                  Just c -> c ++ encode xs
                  Nothing -> error ("No morse code for character: " ++ [x])

isPrefixOf :: String -> String -> Bool
isPrefixOf [] _ = True
isPrefixOf _ [] = False
isPrefixOf (c1:s1) (c2:s2) = c1 == c2 && s1 `isPrefixOf` s2

prefixes msg = [[char] | char <- ['A'..'Z'], (encode [char]) `isPrefixOf` msg]

decode :: String -> [String]
decode "" = [""]
decode msg = [[char] ++ msg'
              | char <- ['A'..'Z'],
                (encode [char]) `isPrefixOf` msg,
                msg' <- decode $ drop (length (encode [char])) msg]