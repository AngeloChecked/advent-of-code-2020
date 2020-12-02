module Day2Spec where


import Day2
import Test.Hspec


spec :: Spec
spec =  
    describe "Day2" $ do
        it "read file lines" $ do
            passwordsToValidate <- fread "resources/passwords.txt" 
            passwordsToValidate `shouldStartWith` ["3-11 z: zzzzzdzzzzlzz","3-7 x: xjxbgpxxgtx","3-4 v: vvmv","3-5 t: tgkfq","9-10 j: jjjjjjjjqjjjj","5-7 r: rnhrhrr","2-6 n: gnntnnsnnqjsbrn"]
        it "password is valid if string contains minimum/maximus given char" $ do
            validPassword (PasswordToValidate 3 11 'w' "wwwdfdsf") `shouldBe` True
            validPassword (PasswordToValidate 4 11 'w' "wwwdfdsf") `shouldBe` False 
            validPassword (PasswordToValidate 1 2 'w' "wwwdfdsf") `shouldBe` False 
            validPassword (PasswordToValidate 1 1 'w' "wdfdsf") `shouldBe` True 
            validPassword (PasswordToValidate 1 3 'w' "wwwdfdsf") `shouldBe` True 
        it "parse string to PasswordToValidate" $ 
            passwordToValidateFromString "13-17 p: pzrpphmphgpkpbppj" `shouldBe` (PasswordToValidate 13 17 'p' "pzrpphmphgpkpbppj")
        it "all valid password" $
            allValidPassword [ (PasswordToValidate 3 11 'w' "wwwdfdsf") 
                             , (PasswordToValidate 4 11 'w' "wwwdfdsf")
                             , (PasswordToValidate 1 2 'w' "wwwdfdsf")
                             , (PasswordToValidate 1 1 'w' "wdfdsf") 
                             , (PasswordToValidate 1 3 'w' "wwwdfdsf") 
                             ]
            `shouldBe` 
                             [ (PasswordToValidate 3 11 'w' "wwwdfdsf") 
                             , (PasswordToValidate 1 1 'w' "wdfdsf") 
                             , (PasswordToValidate 1 3 'w' "wwwdfdsf") 
                             ] 
