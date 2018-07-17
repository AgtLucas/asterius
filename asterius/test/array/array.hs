import Data.Array

foreign import javascript "console.log(${1})" js_print_int :: Int -> IO ()

main :: IO ()
main = do
  let arr = listArray (0, 3) [2, 3, 5, 7]
  js_print_int $ arr ! (3 :: Int)
