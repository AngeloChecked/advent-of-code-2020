module Day3Spec where

import Day3 
import Test.Hspec

spec :: Spec
spec = 
    describe "Day3" $ do
        it "get char in given position, if the index exceeds string size continue from the start" $ do
           check 12 "..##......." `shouldBe` '.'
           check 12 "#...#...#.." `shouldBe` '.'
           check 24 "#...#...#.." `shouldBe` '.'
           check 3 "..#...##...###.........#..#..#." `shouldBe` '.' 
           check 6 "..#...##...###.........#..#..#." `shouldBe` '#' 
           check 30 "..#...##...###.........#..#..#." `shouldBe` '.' 
           check 33 "..#...##...###.........#..#..#." `shouldBe` '#' 
           check 36 "..#...##...###.........#..#..#." `shouldBe` '.' 
        it "give me all char going from top-left 3 times right, 1 down and reapeat" $ do
            traverseLeft3Down1Map map1 `shouldBe` ".....##.#..#..." 
            traverseLeft3Down1Map map2 `shouldBe` ".#.##.####" 
        it "count the number of # encountered in the map" $ do
            numberOfThreesEncountered map1 `shouldBe` 4
            numberOfThreesEncountered map2 `shouldBe` 7 

map1 = [ ".....##.........#..#.......#..."
  , "...........#.........#....###.."
  , "...#.#..#..........#.....#..#.."
  , ".#..###.......##........#.#...."
  , ".#...###.....#..#.#..#...#.##.."
  , ".#...###.....#..#.#..#...#.##.."
  , ".#...###.....#..#.#..#...#.##.."
  , ".#...###.....#..#.#..#...#.##.."
  , ".#...###.....#..#.#..#...#.##.."
  , ".#...###.....#..#.#..#...#.##.."
  , ".#...###.....#..#.#..#...#.##.."
  , ".#...###.....#..#.#..#...#.##.."
  , ".#...###.....#..#.#..#...#.##.."
  , ".#...###.....#..#.#..#...#.##.."
  , ".#...###.....#..#.#..#...#.##.."
  , "##...###.#.#....#......#...#..#" 
  ]

map2 = [ "..##......."
       , "#...#...#.."
       , ".#....#..#."
       , "..#.#...#.#"
       , ".#...##..#."
       , "..#.##....."
       , ".#.#.#....#"
       , ".#........#"
       , "#.##...#..."
       , "#...##....#"
       , ".#..#...#.#"
       ]


