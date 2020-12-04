module Day3Spec where

import Day3 
import Test.Hspec

spec :: Spec
spec = do 
    describe "Day3" $ do
        it "1 is second char, last restart" $ do
           check 11 "#...#...#.." `shouldBe` '#'
           check 1 "#...#...#.." `shouldBe` '.'
        it "get char in given position, if the index exceeds string size continue from the start" $ do
           check 12 "..##......." `shouldBe` '.'
           check 12 "#...#...#.." `shouldBe` '.'
           check 24 "#...#...#.." `shouldBe` '.'
           check 3 "..#...##...###.........#..#..#." `shouldBe` '.' 
           check 6 "..#...##...###.........#..#..#." `shouldBe` '#' 
           check 30 "..#...##...###.........#..#..#." `shouldBe` '.' 
           check 33 "..#...##...###.........#..#..#." `shouldBe` '#' 
           check 36 "..#...##...###.........#..#..#." `shouldBe` '.' 
           check 232222423 "..#...##...###.........#..#..#." `shouldBe` '.' 
        it "give me all char going from top-left 3 times right, 1 down and reapeat" $ do
            traverseLeft3Down1Map map1 `shouldBe` ".....##.#..#..." 
            traverseLeft3Down1Map map2 `shouldBe` ".#.##.####" 
        it "count the number of # encountered in the map" $ do
            numberOfThreesEncountered map1 `shouldBe` 4
            numberOfThreesEncountered map2 `shouldBe` 7 
    describe "Part2" $ do
        it "give me all char going from top-left x times right, y time down and reapeat" $ do
            traverseLeftDownMap 3 1 map1 `shouldBe` ".....##.#..#..." 
            traverseLeftDownMap 3 1 map2 `shouldBe` ".#.##.####" 
        it "count the number of # encountered in the map with given moving right-down coordinate pattern" $ do
            numberOfThreesEncountered2 1 1 map2 `shouldBe` 2
            numberOfThreesEncountered2 3 1 map2 `shouldBe` 7
            numberOfThreesEncountered2 5 1 map2 `shouldBe` 3
            numberOfThreesEncountered2 7 1 map2 `shouldBe` 4
            numberOfThreesEncountered2 1 2 map2 `shouldBe` 2
            numberOfThreesEncountered2 3 1 map1 `shouldBe` 4
        it "product all ecountered # in all different patterns" $
            productEnconuteredThressWithDifferentsPatterns [(1,1),(3,1),(5,1),(7,1),(1,2)] map2 `shouldBe` 336 

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


