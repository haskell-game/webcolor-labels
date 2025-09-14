{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE BlockArguments #-}

{-# OPTIONS_GHC -Wno-orphans #-}

module Main (main) where

import WebColor.Labels
import Data.Word (Word8)
import GHC.OverloadedLabels

import Test.Tasty
import Test.Tasty.HUnit

instance IsWebColor "gold" where
  webColor k = k 255 215 0

instance IsWebColorAlpha "gold" where
  webColorAlpha k = k 255 215 0 255

data RGB = MkRGB Word8 Word8 Word8
  deriving (Show, Eq)

instance IsWebColor s => IsLabel s RGB where
  fromLabel = webColor @s MkRGB

data RGBA = MkRGBA Word8 Word8 Word8 Word8
  deriving (Show, Eq)

instance IsWebColorAlpha s => IsLabel s RGBA where
  fromLabel = webColorAlpha @s MkRGBA

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [ testGroup "Named colors RGB" [
      testCase "white" do #white @?= MkRGB 255 255 255; #FFFFFF @?= MkRGB 255 255 255,
      testCase "silver" do #silver @?= MkRGB 192 192 192; #C0C0C0 @?= MkRGB 192 192 192,
      testCase "gray" do #gray @?= MkRGB 128 128 128; #808080 @?= MkRGB 128 128 128,
      testCase "black" do #black @?= MkRGB 0 0 0; #000000 @?= MkRGB 0 0 0,
      testCase "red" do #red @?= MkRGB 255 0 0; #FF0000 @?= MkRGB 255 0 0,
      testCase "maroon" do #maroon @?= MkRGB 128 0 0; #800000 @?= MkRGB 128 0 0,
      testCase "yellow" do #yellow @?= MkRGB 255 255 0; #FFFF00 @?= MkRGB 255 255 0,
      testCase "olive" do #olive @?= MkRGB 128 128 0; #808000 @?= MkRGB 128 128 0,
      testCase "lime" do #lime @?= MkRGB 0 255 0; #00FF00 @?= MkRGB 0 255 0,
      testCase "green" do #green @?= MkRGB 0 128 0; #008000 @?= MkRGB 0 128 0,
      testCase "aqua" do #aqua @?= MkRGB 0 255 255; #00FFFF @?= MkRGB 0 255 255,
      testCase "teal" do #teal @?= MkRGB 0 128 128; #008080 @?= MkRGB 0 128 128,
      testCase "blue" do #blue @?= MkRGB 0 0 255; #0000FF @?= MkRGB 0 0 255,
      testCase "navy" do #navy @?= MkRGB 0 0 128; #000080 @?= MkRGB 0 0 128,
      testCase "fuchsia" do #fuchsia @?= MkRGB 255 0 255; #FF00FF @?= MkRGB 255 0 255,
      testCase "purple" do #purple @?= MkRGB 128 0 128; #800080 @?= MkRGB 128 0 128
    ]
    , testGroup "Named colors RGBA" [
      testCase "white" do #white @?= MkRGBA 255 255 255 255; #FFFFFF @?= MkRGBA 255 255 255 255,
      testCase "silver" do #silver @?= MkRGBA 192 192 192 255; #C0C0C0 @?= MkRGBA 192 192 192 255,
      testCase "gray" do #gray @?= MkRGBA 128 128 128 255; #808080 @?= MkRGBA 128 128 128 255,
      testCase "black" do #black @?= MkRGBA 0 0 0 255; #000000 @?= MkRGBA 0 0 0 255,
      testCase "red" do #red @?= MkRGBA 255 0 0 255; #FF0000 @?= MkRGBA 255 0 0 255,
      testCase "maroon" do #maroon @?= MkRGBA 128 0 0 255; #800000 @?= MkRGBA 128 0 0 255,
      testCase "yellow" do #yellow @?= MkRGBA 255 255 0 255; #FFFF00 @?= MkRGBA 255 255 0 255,
      testCase "olive" do #olive @?= MkRGBA 128 128 0 255; #808000 @?= MkRGBA 128 128 0 255,
      testCase "lime" do #lime @?= MkRGBA 0 255 0 255; #00FF00 @?= MkRGBA 0 255 0 255,
      testCase "green" do #green @?= MkRGBA 0 128 0 255; #008000 @?= MkRGBA 0 128 0 255,
      testCase "aqua" do #aqua @?= MkRGBA 0 255 255 255; #00FFFF @?= MkRGBA 0 255 255 255,
      testCase "teal" do #teal @?= MkRGBA 0 128 128 255; #008080 @?= MkRGBA 0 128 128 255,
      testCase "blue" do #blue @?= MkRGBA 0 0 255 255; #0000FF @?= MkRGBA 0 0 255 255,
      testCase "navy" do #navy @?= MkRGBA 0 0 128 255; #000080 @?= MkRGBA 0 0 128 255,
      testCase "fuchsia" do #fuchsia @?= MkRGBA 255 0 255 255; #FF00FF @?= MkRGBA 255 0 255 255,
      testCase "purple" do #purple @?= MkRGBA 128 0 128 255; #800080 @?= MkRGBA 128 0 128 255
    ]
  , testGroup "Custom colors" [
      testCase "rgb gold" do #gold @?= MkRGB 255 215 0; #FFD700 @?= MkRGB 255 215 0,
      testCase "rgba gold" do #gold @?= MkRGBA 255 215 0 255; #FFD700 @?= MkRGBA 255 215 0 255
    ]
  , testGroup "hex syntax" [
      testCase "rgb 3 == 6 chars"  $ #fad    `rgb`  #ffaadd,
      testCase "rgb 3 == 8 chars"  $ #fad    `rgb`  #ffaaddff,
      testCase "rgb 4 == 8 chars"  $ #eadf   `rgb`  #eeaaddff,
      testCase "rgb 6 == 8 chars"  $ #fade12 `rgb`  #fade12ff,
      testCase "rgba 3 == 6 chars" $ #fad    `rgba` #ffaadd,
      testCase "rgba 3 == 8 chars" $ #fad    `rgba` #ffaaddff,
      testCase "rgba 4 == 8 chars" $ #fade   `rgba` #ffaaddee,
      testCase "rgba 6 == 8 chars" $ #fade12 `rgba` #fade12ff
    ]
  ]
  where
    rgb :: RGB -> RGB -> Assertion
    rgb = (@?=)

    rgba :: RGBA -> RGBA -> Assertion
    rgba = (@?=)
