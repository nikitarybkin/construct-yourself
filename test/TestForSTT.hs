{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Construction     (Context (..), Name, Substitution (..), Term (..), Type (..), substitute1, compose, termP)
import           Data.Text        hiding (singleton)
import           Tasks
import           Test.Hspec
import qualified Text.Parsec      as TP (parse)
import           Text.Parsec.Text
import           Data.Map
import qualified Data.Set         as S (fromList)
import qualified Data.Map                        as M ((!), empty)

main :: IO ()
main = do

  hspec $ do
    describe "isMonoid test for Context" isMonoidContextTest
    describe "isMonoid test for Substitution" isMonoidSubstitutionTest
    describe "doesCompositionWork test for substitute" doesCompositionWorkTest
    describe "doesSubstituteFunctionWork test for both Type and Context" doesSubstituteFunctionWorkTest
--    describe "doesParserParseBrackets test for Parser" doesParserParseBracketsTest
--    describe "doesParserParseLambdas test for Parser" doesParserParseLambdasTest


context1 = Context (singleton "x" (TVar "x1"))
context2 = Context (singleton "y" (TVar "y1"))
context3 = Context (singleton "z" (TVar "z1"))
context4 = Context (singleton "x" (TVar "x1") `mappend` singleton "y" (TVar "y1"))

isMonoidContextTest :: SpecWith ()
isMonoidContextTest = do
  it "#1" $ (mempty) `shouldBe` Context M.empty
  it "#2" $ (context1 `mappend` mempty) `shouldBe` context1
  it "#3" $ (mempty `mappend` context1) `shouldBe` context1
  it "#4" $ (context1 `mappend` context2)`mappend` context3 `shouldBe` context1 `mappend` (context2 `mappend` context3)
  it "#5" $ (context1 `mappend` context2) `shouldBe` context4
  
substitution1 = Substitution (singleton "x" (TVar "x1"))
substitution2 = Substitution (singleton "y" (TVar "y1"))
substitution3 = Substitution (singleton "z" (TVar "z1"))
substitution4 = Substitution (singleton "x" (TVar "x1") `mappend` singleton "y" (TVar "y1"))

isMonoidSubstitutionTest :: SpecWith ()
isMonoidSubstitutionTest = do
  it "#1" $ (mempty) `shouldBe` Substitution M.empty
  it "#2" $ (mempty `mappend` substitution1) `shouldBe` substitution1
  it "#3" $ (substitution1 `mappend` mempty) `shouldBe` substitution1
  it "#4" $ (substitution1 `mappend` substitution2)`mappend` substitution3 `shouldBe` substitution1 `mappend` (substitution2 `mappend` substitution3)
  it "#5" $ (substitution1 `mappend` substitution2) `shouldBe` substitution4

substitutionA = Substitution $ fromList [("x1", TVar "x2"), ("x3", TVar "x4")]
substitutionB = Substitution $ fromList [("x2", TVar "a"), ("x4", TVar "b")]
substitutionAB = Substitution $ fromList [("x1", TVar "a"), ("x3", TVar "b"), ("x2", TVar "a"), ("x4", TVar "b")]
substitutionBA = Substitution $ fromList [("x1", TVar "x2"), ("x3", TVar "x4"), ("x2", TVar "a"), ("x4", TVar "b")]
substitutionC = Substitution $ fromList [("x1", TVar "x2"), ("x3", TVar "x4"), ("x5", TVar "x6")]

doesCompositionWorkTest :: SpecWith ()
doesCompositionWorkTest = do
  it "#1" $ substitutionA `compose` substitutionB `shouldBe` substitutionAB
  it "#2" $ substitutionB `compose` substitutionA `shouldBe` substitutionBA
  it "#3" $ (Substitution M.empty) `compose` substitutionA `shouldBe` substitutionA
  it "#4" $ substitutionA `compose` (Substitution M.empty) `shouldBe` substitutionA
  it "#5" $ substitutionA `compose` substitutionC `shouldBe` substitutionC

sub1 = Substitution $ fromList [("x1", TVar "a1"), ("x2", TVar "a2")]
type1 = TArr (TVar "x2") (TArr (TVar "b1") (TVar "x1"))
type2 = TArr (TVar "a2") (TArr (TVar "b1") (TVar "a1"))
--con1 = Context M.empty "x" type1
--con2 = Context M.empty "x" type2

doesSubstituteFunctionWorkTest :: SpecWith()
doesSubstituteFunctionWorkTest = do
  it "#1" $ substitute1 sub1 type1 `shouldBe` type2
--  it "#4" $ substitute1 sub1 con1 `shouldBe` con2

--combined1 = App (Lam "x" (Var "x")) (Var "y")
--combined2 = Lam "z" (App (Lam "x" (Var "x")) (Var "y"))

--doesParserParseBracketsTest :: SpecWith()
--doesParserParseBracketsTest = do
--  it "#1" $ (termP "((\\x.x)  (y))") `shouldBe` combined1 
--  it "#2" $ (termP "((((\\z.((\\x.x) y)))))") `shouldBe` combined2

--lam1 = Lam "x" (Var "x")
--lam2 = Lam "x" (Var "y")
--lam3 = Lam "x" (App (Var "x") (Var "y"))

--doesParserParseLambdasTest :: SpecWith()
--doesParserParseLambdasTest = do
-- it "#1" $ (termP "(\\x.x)") `shouldBe` lam1
-- it "#2" $ (termP "(\\x.y)") `shouldBe` lam2
-- it "#3" $ (termP "(\\x.(x y))") `shouldBe` lam3
-- it "#4" $ (termP "((\\x.x) y)") `shouldBe` combined1

