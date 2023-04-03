module Main (main) where

import Lib (run)

main :: IO ()
main = do
    print .run $ "assume double: int -> int\nlet h = handler double (val x: int -> x) ((x: int, k: int -> int{}) -> (k (k x))) in with h handle (perform double 3 + 10)" -- Right (Int 23)
    print .run $ "assume double: int -> int\nassume doublee: int -> int\nlet h = handler double (val x: int -> x) ((x: int, k: int -> int{}) -> (k (x + 1))) in with h handle let hh = handler doublee (val x: int -> (x + perform double 2)) ((x: int, k: int -> int{double}) -> (k (x + 1))) in with hh handle (10 + perform doublee 3)" -- Right (Int 17)
