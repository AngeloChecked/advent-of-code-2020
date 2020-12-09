module Day4Spec where

import Day4
import Day2
import Test.Hspec


fileStart = ["eyr:2028 iyr:2016 byr:1995 ecl:oth","pid:543685203 hcl:#c0946f","hgt:152cm","cid:252","","hcl:#733820 hgt:155cm","iyr:2013 byr:1989 pid:728471979","ecl:grn eyr:2022","","hgt:171cm","iyr:2013 pid:214368857 hcl:#cfa07d byr:1986 eyr:2028 ecl:grn",""]

spec :: Spec
spec = do
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
    describe "Part2" $ do
        it "split start and last 2" $
            takeLast 2 "3424cm" `shouldBe` ("3424", "cm")
        it "valid Measures" $ do
            validMeasures "59in" `shouldBe` Just "59in"
            validMeasures "150cm" `shouldBe` Just "150cm"
            validMeasures "190in" `shouldBe` Nothing 
            validMeasures "190" `shouldBe` Nothing 
        it "valid color" $ do
            validColor "#3af343" `shouldBe` Just "#3af343"
            validColor "#3af34" `shouldBe` Nothing 
            validColor "#3afg44" `shouldBe` Nothing 
            validColor "#3afb.4" `shouldBe` Nothing 
            validColor "%3afb34" `shouldBe` Nothing 
        it "valid eye color" $ do
            validEyeColor "amb" `shouldBe` Just "amb" 
            validEyeColor "blu" `shouldBe` Just "blu"
            validEyeColor "brn" `shouldBe` Just "brn"
            validEyeColor "gry" `shouldBe` Just "gry"
            validEyeColor "grn" `shouldBe` Just "grn"
            validEyeColor "hzl" `shouldBe` Just "hzl"
            validEyeColor "oth" `shouldBe` Just "oth"
            validEyeColor "re" `shouldBe` Nothing 
        it "valid pin" $ do
            validPid "000434434" `shouldBe` Just "000434434" 
            validPid "000" `shouldBe` Nothing 
            validPid "00043343q" `shouldBe` Nothing 
            validPid "000000001" `shouldBe` Just "000000001" 
            validPid "000000000" `shouldBe` Nothing
        it "parse passport" $
            parsePassport2 ["eyr:2028","iyr:2016","byr:1995","ecl:oth","pid:543685203","hcl:#c0946f","hgt:152cm","cid:252"]
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
        it "parse invalid passport" $
            parsePassport2 ["eyr:2028","iyr:2016","byr:1995","ecl:oth","pid:543685203","hcl:#c0946f","hgt:152vh","cid:252"]
            `shouldBe`
            Nothing
        it "4 valid passports 4 invalid" $
            length (parseAllValidPassports2 valid4Invalid4) 
            `shouldBe`
            4

valid4Invalid4 =
    [ ["eyr:1972","cid:100","hcl:#18171d","ecl:amb","hgt:170","pid:186cm","iyr:2018","byr:1926"]
    , ["iyr:2019","hcl:#602927","eyr:1967","hgt:170cm","ecl:grn","pid:012533040","byr:1946"]
    , ["hcl:dab227","iyr:2012","ecl:brn","hgt:182cm","pid:021572410","eyr:2020","byr:1992","cid:277"]
    , ["hgt:59cm","ecl:zzz","eyr:2038","hcl:74454a","iyr:2023","pid:3556412378","byr:2007"]
    
    , ["pid:087499704","hgt:74in","ecl:grn","iyr:2012","eyr:2030","byr:1980","hcl:#623a2f"]
    , ["eyr:2029","ecl:blu","cid:129","byr:1989","iyr:2014","pid:896056539","hcl:#a97842","hgt:165cm"]
    , ["hcl:#888785","hgt:164cm","byr:2001","iyr:2015","cid:88","pid:545766238","ecl:hzl","eyr:2022"]
    , ["iyr:2010","hgt:158cm","hcl:#b6652a","ecl:blu","byr:1944","eyr:2021","pid:093154719"]
    ]
