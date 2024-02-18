module SpellingCorrector

import Trie


public export
data SpellingCorrector = SC Trie

public export
newCorrector : SpellingCorrector
newCorrector = SC (makeTrie '0')

public export
loadDictionary : String -> SpellingCorrector -> SpellingCorrector
loadDictionary file sc = foldl (\(SC trie), line => SC (addString (trim (toLower line)) trie)) sc (lines file)


public export
suggestSimilarWord : String -> SpellingCorrector -> Maybe String
suggestSimilarWord input (SC trie) = let lower_input = toLower input in
    case findString lower_input trie of 
        Just trie => Just lower_input
        Nothing => let edit_dist1 = genEditDist1 lower_input in
            let possible_words = filter (\word => isJust Trie.findString word trie) edit_dist1 in
                case length possible_words of
                    0 => let edit_dist2 = genEditDist2 edit_dist1 in
                        let possible_words = filter (\word => isJust Trie.findString word trie) edit_dist2 in
                            case length possible_words of 
                                0 => Nothing
                                _ => let nodes = map (\word => (word, Trie.findString word trie)) possible_words in
                                    let node = foldl unjustNode [] nodes in
                                        Just (fst node)
                    _ => let possible_words = filter (\word => isJust Trie.findString word trie) edit_dist1 in
                        case length possible_words of 
                            0 => Nothing
                            _ => let nodes = map (\word => (word, Trie.findString word trie)) possible_words in
                                let node = foldl unjustNode [] nodes in
                                    Just (fst node)

unjustNode : (String, Trie) -> (String, Maybe Trie) -> (String, Trie)
unjustNode acc node = case (snd node) of 
    Just x => let (TrieNode _ freq_new _) = x in
        let (TrieNode _ freq_old _) = (snd acc) in 
            if freq_new > freq_old then (fst node, x) else acc
    _ => acc   

genEditDist1 : String -> List String
genEditDist1 input = (union (union (union (genDeleteChar input) (genTransposeChar input)) (genAltChar input)) (genInsertChar input))

genDeleteChar : String -> List String
genDeleteChar input = genDeleteHelper (unpack input) 0 where
    genDeleteHelper : List Char -> Nat -> List String
    genDeleteHelper input index = if length input == index then [] 
        else (pack (take index input ++ drop (index + 1) input)) :: genDeleteHelper input (index + 1)

genTransposeChar : String -> List String
genTransposeChar input = let input = unpack input in 
    genTransposeHelperI 0 where
        genTransposeHelperI : Nat -> List String
        genTransposeHelperI i = if length input == i then []
            else genTransposeHelperJ i (i + 1) ++ genTransposeHelperI (i + 1) where
                genTransposeHelperJ : Nat -> Nat -> List String
                genTransposeHelperJ i j = if length input == j then []
                    else (pack (replaceAt j (index i input) (replaceAt i (index j input) input))) :: genTransposeHelperJ i (j + 1)


alphabet : List Char
alphabet = unpack "abcdefghijklmnopqrstuvwxyz"

genAltChar : String -> List String
genAltChar input = let input = unpack input in
    genAltHelper 0 where
        genAltHelper : Nat -> List String
        genAltHelper index = if length input == index then []
            else (addAlpha alphabet) ++ genAltHelper (index + 1) where
                addAlpha : List Char -> List String
                addAlpha [] = []
                addAlpha (c :: alph) = ((pack (replaceAt index c input)) :: (addAlpha alph))