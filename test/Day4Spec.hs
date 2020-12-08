module Day4Spec where

import Day4
import Day2
import Test.Hspec


fileStart = ["eyr:2028 iyr:2016 byr:1995 ecl:oth","pid:543685203 hcl:#c0946f","hgt:152cm","cid:252","","hcl:#733820 hgt:155cm","iyr:2013 byr:1989 pid:728471979","ecl:grn eyr:2022","","hgt:171cm","iyr:2013 pid:214368857 hcl:#cfa07d byr:1986 eyr:2028 ecl:grn",""]

spec :: Spec
spec = 
    describe "Day 4" $ do
        it "read file lines" $ do
            passports <- fread "resources/passports.txt" 
            passports `shouldStartWith` fileStart
        it "split spaces and flat" $ 
            splitSpaceAndFlat [["fsaf fadsf","sfsf","sdfas fsaf"]] `shouldBe` [["fsaf","fadsf","sfsf","sdfas","fsaf"]]
        it "split all passport and flat" $ 
            (splitSpaceAndFlat $ splitPassportsRawFromBatch fileStart)
            `shouldBe` 
            [["eyr:2028","iyr:2016","byr:1995","ecl:oth","pid:543685203","hcl:#c0946f","hgt:152cm","cid:252"],["hcl:#733820","hgt:155cm","iyr:2013","byr:1989","pid:728471979","ecl:grn","eyr:2022"],["hgt:171cm","iyr:2013","pid:214368857","hcl:#cfa07d","byr:1986","eyr:2028","ecl:grn"],[]]
        it "parse passport" $
            parsePassport ["eyr:2028","iyr:2016","byr:1995","ecl:oth","pid:543685203","hcl:#c0946f","hgt:152cm","cid:252"]
            `shouldBe`
            Just Passport { eyr = "2028"
                     , iyr = "2016"
                     , byr = "1995"
                     , ecl = "oth"
                     , pid = "543685203" 
                     , hcl = "#c0946f" 
                     , hgt = "152cm"
                     , cid = Just "252"
                     } 
        it "passport without cid is valid" $
            parsePassport ["eyr:2028","iyr:2016","byr:1995","ecl:oth","pid:543685203","hcl:#c0946f","hgt:152cm"]
            `shouldBe`
            Just Passport { eyr = "2028"
                     , iyr = "2016"
                     , byr = "1995"
                     , ecl = "oth"
                     , pid = "543685203" 
                     , hcl = "#c0946f" 
                     , hgt = "152cm"
                     , cid = Nothing 
                     } 
        it "invalid passport" $
            parsePassport ["hcl:#c0946f","hgt:152cm","cid:252"] `shouldBe` Nothing
        it "parse all valid passports" $ 
            parseAllValidPassports (splitSpaceAndFlat $ splitPassportsRawFromBatch fileStart) 
            `shouldBe`
            [Passport {byr = "1995", iyr = "2016", eyr = "2028", hgt = "152cm", hcl = "#c0946f", ecl = "oth", pid = "543685203", cid = Just "252"},Passport {byr = "1989", iyr = "2013", eyr = "2022", hgt = "155cm", hcl = "#733820", ecl = "grn", pid = "728471979", cid = Nothing},Passport {byr = "1986", iyr = "2013", eyr = "2028", hgt = "171cm", hcl = "#cfa07d", ecl = "grn", pid = "214368857", cid = Nothing}]  
