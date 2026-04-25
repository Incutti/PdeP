module Spec where
import PdePreludat
import Library
import Test.Hspec

correrTests :: IO ()
correrTests = hspec $ do
  describe "Ejercicio a" $ do
    it "promedio de hojas es 475.75" $ do
       promedioDeHojas biblioteca `shouldBe` 475.75

  describe "Ejercicio B" $ do
    it "fundacion es lectura obligatoria" $ do
       lecturaObligatoria fundacion `shouldBe` True
  
  describe "Ejercicio C" $ do
    it "la biblioteca es fantasiosa" $ do
       esBibliotecaFantasiosa biblioteca `shouldBe` True
  
  describe "Ejercicio D" $ do
    it "el nombre de la biblioteca está bien" $ do
       nombreDeLaBiblioteca biblioteca `shouldBe` "lvstntshngknkyjnfndcnsndmnrgnldstbrsgnrlgd"
  
  describe "Ejercicio E" $ do
    it "es biblioteca ligera" $ do
       esBibliotecaLigera biblioteca `shouldBe` False
  

