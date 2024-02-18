module Trie

import Data.Vect
import Data.String
import Data.Fin

char_to_index : Char -> Fin 26
char_to_index 'a' = 0 
char_to_index 'b' = 1 
char_to_index 'c' =  2 
char_to_index 'd' =  3  
char_to_index 'e' =  4  
char_to_index 'f' =  5  
char_to_index 'g' =  6  
char_to_index 'h' =  7  
char_to_index 'i' =  8  
char_to_index 'j' =  9  
char_to_index 'k' =  10  
char_to_index 'l' =  11  
char_to_index 'm' =  12  
char_to_index 'n' =  13  
char_to_index 'o' =  14  
char_to_index 'p' =  15  
char_to_index 'q' =  16  
char_to_index 'r' =  17  
char_to_index 's' =  18  
char_to_index 't' =  19  
char_to_index 'u' =  20  
char_to_index 'v' =  21  
char_to_index 'w' =  22  
char_to_index 'x' =  23  
char_to_index 'y' =  24  
char_to_index 'z' =  25  
char_to_index c =  0  

public export
data Trie = TrieNode Char Nat (Vect 26 (Maybe Trie))

public export
insertAtPos : Fin len -> Trie -> (Vect len (Maybe Trie)) -> (Vect len (Maybe Trie))
insertAtPos pos val vec = replaceAt pos (Just val) vec

public export
makeTrie : Char -> Trie
makeTrie char = TrieNode char 0 (replicate 26 Nothing)

incrementTrie : Trie -> Trie
incrementTrie (TrieNode char freq children) = TrieNode char (freq + 1) children

public export
addString : String -> Trie -> Trie
addString str tr = add_string_helper (unpack str) tr where 
    add_string_helper : (List Char) -> Trie -> Trie
    add_string_helper Prelude.List.Nil trie = incrementTrie trie
    add_string_helper (char :: str) (TrieNode c freq children) = case Data.Vect.index (char_to_index char) children of
        Nothing => TrieNode c freq (insertAtPos (char_to_index char) (add_string_helper str (makeTrie char)) children)
        Just child_trie => TrieNode c freq (insertAtPos (char_to_index char) (add_string_helper str (makeTrie char)) children)


public export
findString : String -> Trie -> Maybe Trie
findString str tr = findStringHelper (unpack str) tr where
    findStringHelper : (List Char) -> Trie -> Maybe Trie
    findStringHelper Prelude.List.Nil trie = let (TrieNode _ freq _) = trie in
        if freq >= 1 then Just trie else Nothing
    findStringHelper (char :: str) trie = let (TrieNode _ _ children) = trie in
        case Data.Vect.index (char_to_index char) children of
            Just child_trie => findStringHelper str child_trie
            x => x

{-Show Trie where
    show (TrieNode _ _ children) = concatMap (showHelper "") children where
        showHelper : String -> Maybe Trie -> String
        showHelper str Nothing = ""
        showHelper str (Just trie) = let (TrieNode char freq children) = trie in
        if freq trie >= 1 then holder ++ -}