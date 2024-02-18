import Trie



main : IO ()
main = let trie = Trie.makeTrie '0' in
        let trie = Trie.addString "hello" trie in
            case Trie.findString "hello" trie of
                Nothing => putStrLn "Nothing"
                Just x => putStrLn "found hello"
    {-args <- getArgs
            print "Hello World" -}