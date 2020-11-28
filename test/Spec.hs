import Test.Tasty
import Test.Tasty.HUnit
import Lists

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "All tests" [listsTests]

listsTests = testGroup "Lists tests"
  [ testCase "P01 Find the last element of a list" $ do
    (last' [1, 2, 3, 4]) @=? 4
    (last' ['x', 'y', 'z']) @=? 'z'
  , testCase "P02 Find the last but one element of a list" $ do
      (penultimate [1, 2, 3, 4] @=? 3)
      (penultimate ['a'..'z'] @=? 'y')
  , testCase "P03 Find the k'th element of a list. The first element in the list is number 1" $ do
      (elementAt [1, 2, 3, 4] 2) @=? 2
      (elementAt "haskell" 5) @=? 'e'
  , testCase "P04 Find the number of elements of a list" $ do
      (myLength [123, 456, 789]) @=? 3
      (myLength "Hello, world!" @=? 13)
   , testCase "P05 Reverse a list" $ do
       (myReverse "A man, a plan, a canal, panama!") @=? "!amanap ,lanac a ,nalp a ,nam A"
       (myReverse [1,2,3,4]) @=? [4,3,2,1]
  ]
