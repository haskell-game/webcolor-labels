{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
#if __GLASGOW_HASKELL__ >= 9101
{-# LANGUAGE RequiredTypeArguments #-}
#endif

module WebColor.Labels
  ( WebColorParsed,
    ParseWebColorMaybeAlpha,
    WebColor,
    ParseWebColor,
    WebColorAlpha,
    ParseWebColorAlpha,
    IsWebColor(..),
#if __GLASGOW_HASKELL__ >= 9101
    webColor,
#endif
    IsWebColorAlpha(..),
#if __GLASGOW_HASKELL__ >= 9101
    webColorAlpha,
#endif
    ParseChar,
  ) where

import Data.Proxy (Proxy (..))
import Data.Type.Equality (type (~))
import Data.Word (Word8)
import GHC.TypeLits
  (ErrorMessage (..), KnownNat, Nat, Symbol, TypeError, UnconsSymbol, natVal, type (*), type (+))
import Prelude (Char, Maybe (..), fromIntegral, ($!))

type WebColorParsed = (Nat, Nat, Nat, Maybe Nat)

type ParseWebColorMaybeAlpha :: Symbol -> WebColorParsed
type family ParseWebColorMaybeAlpha s where
  ParseWebColorMaybeAlpha "white"   = '(255, 255, 255, Nothing)
  ParseWebColorMaybeAlpha "silver"  = '(192, 192, 192, Nothing)
  ParseWebColorMaybeAlpha "gray"    = '(128, 128, 128, Nothing)
  ParseWebColorMaybeAlpha "black"   = '(0,   0,   0,   Nothing)
  ParseWebColorMaybeAlpha "red"     = '(255, 0,   0,   Nothing)
  ParseWebColorMaybeAlpha "maroon"  = '(128, 0,   0,   Nothing)
  ParseWebColorMaybeAlpha "yellow"  = '(255, 255, 0,   Nothing)
  ParseWebColorMaybeAlpha "olive"   = '(128, 128, 0,   Nothing)
  ParseWebColorMaybeAlpha "lime"    = '(0,   255, 0,   Nothing)
  ParseWebColorMaybeAlpha "green"   = '(0,   128, 0,   Nothing)
  ParseWebColorMaybeAlpha "aqua"    = '(0,   255, 255, Nothing)
  ParseWebColorMaybeAlpha "teal"    = '(0,   128, 128, Nothing)
  ParseWebColorMaybeAlpha "blue"    = '(0,   0,   255, Nothing)
  ParseWebColorMaybeAlpha "navy"    = '(0,   0,   128, Nothing)
  ParseWebColorMaybeAlpha "fuchsia" = '(255, 0,   255, Nothing)
  ParseWebColorMaybeAlpha "purple"  = '(128, 0,   128, Nothing)
  ParseWebColorMaybeAlpha s = ParseColorRec '[] (UnconsSymbol s)

type ParseColorRec :: [Nat] -> Maybe (Char, Symbol) -> WebColorParsed
type family ParseColorRec color str where
  ParseColorRec colors Nothing = UpgradeColor colors
  ParseColorRec colors (Just '(ch, t)) = ParseColorRec (ParseChar ch : colors) (UnconsSymbol t)

type WebColor = (Nat, Nat, Nat)

type ParseWebColor :: Symbol -> WebColor
type family ParseWebColor s where
  ParseWebColor s = ParseRGBColorWorker (ParseWebColorMaybeAlpha s)

type ParseRGBColorWorker :: WebColorParsed -> WebColor
type family ParseRGBColorWorker color where
  ParseRGBColorWorker '(r, g, b, Nothing)  = '(r, g, b)
  ParseRGBColorWorker '(r, g, b, Just 255) = '(r, g, b)
  ParseRGBColorWorker '(_, _, _, Just _) = TypeError (Text "Unexpected alpha channel! RGB color expected")

type WebColorAlpha = (Nat, Nat, Nat, Nat)

type ParseWebColorAlpha :: Symbol -> WebColorAlpha
type family ParseWebColorAlpha s where
  ParseWebColorAlpha s = ParseRGBAColorWorker (ParseWebColorMaybeAlpha s)

type ParseRGBAColorWorker :: WebColorParsed -> WebColorAlpha
type family ParseRGBAColorWorker color where
  ParseRGBAColorWorker '(r, g, b, Nothing) = '(r, g, b, 0xFF)
  ParseRGBAColorWorker '(r, g, b, Just a)  = '(r, g, b, a)

type a & b = a

class IsWebColor s where
  webColorProxy ::
    Proxy s ->
    (Word8 & "red" -> Word8 & "green" -> Word8 & "blue" -> r) -> r

#if __GLASGOW_HASKELL__ >= 9101
webColor ::
  forall s -> IsWebColor s =>
  (Word8 & "red" -> Word8 & "green" -> Word8 & "blue" -> r) -> r
webColor s = webColorProxy @s Proxy
#endif

-- | Hacky instance to avoid a warning from GHC
instance {-# OVERLAPPING #-} IsWebColor "red" where
  webColorProxy _ k = k 255 0 0

instance {-# OVERLAPPABLE #-}
  ( ParseWebColor s ~ '(r, g, b),
    KnownNat r, KnownNat g, KnownNat b
  ) => IsWebColor s where
  webColorProxy _ k =
      k `color` Proxy @r `color` Proxy @g `color` Proxy @b

class IsWebColorAlpha s where
  webColorAlphaProxy ::
    Proxy s ->
    (Word8 & "red" -> Word8 & "green" -> Word8 & "blue" -> Word8 & "alpha" -> r) -> r

#if __GLASGOW_HASKELL__ >= 9101
webColorAlpha ::
  forall s -> IsWebColorAlpha s =>
  (Word8 & "red" -> Word8 & "green" -> Word8 & "blue" -> Word8 & "alpha" -> r) -> r
webColorAlpha s = webColorAlphaProxy @s Proxy
#endif

-- | Hacky instance to avoid a warning from GHC
instance {-# OVERLAPPING #-} IsWebColorAlpha "red" where
  webColorAlphaProxy _ k = k 255 0 0 255

instance {-# OVERLAPPABLE #-}
  ( ParseWebColorAlpha s ~ '(r, g, b, a),
    KnownNat r, KnownNat g, KnownNat b, KnownNat a
  ) => IsWebColorAlpha s where
  webColorAlphaProxy _ k =
      k `color` Proxy @r `color` Proxy @g `color` Proxy @b `color` Proxy @a

color :: KnownNat n => (Word8 -> r) -> Proxy n -> r
color k p = k $! fromIntegral (natVal p)

type UpgradeColor :: [Nat] -> WebColorParsed
type family UpgradeColor nats where
  UpgradeColor [        b,      g,      r     ] = UpgradeColor [      b,  b,  g,  g,  r,  r]
  UpgradeColor [a,      b,      g,      r     ] = UpgradeColor [a, a, b,  b,  g,  g,  r,  r]
  UpgradeColor [        b1, b2, g1, g2, r1, r2] = '(MakeByte r2 r1, MakeByte g2 g1, MakeByte b2 b1, Nothing)
  UpgradeColor [a1, a2, b1, b2, g1, g2, r1, r2] = '(MakeByte r2 r1, MakeByte g2 g1, MakeByte b2 b1, Just (MakeByte a2 a1))
  UpgradeColor _ = TypeError (Text "Unexpected number of hex codes" :$$: Text "expected 3, 4, 6 or 8")

type ParseChar :: Char -> Nat
type family ParseChar ch where
  ParseChar '0' = 0x0
  ParseChar '1' = 0x1
  ParseChar '2' = 0x2
  ParseChar '3' = 0x3
  ParseChar '4' = 0x4
  ParseChar '5' = 0x5
  ParseChar '6' = 0x6
  ParseChar '7' = 0x7
  ParseChar '8' = 0x8
  ParseChar '9' = 0x9
  ParseChar 'a' = 0xa
  ParseChar 'b' = 0xb
  ParseChar 'c' = 0xc
  ParseChar 'd' = 0xd
  ParseChar 'e' = 0xe
  ParseChar 'f' = 0xf
  ParseChar 'A' = 0xA
  ParseChar 'B' = 0xB
  ParseChar 'C' = 0xC
  ParseChar 'D' = 0xD
  ParseChar 'E' = 0xE
  ParseChar 'F' = 0xF
  ParseChar ch  = TypeError (Text "Unable to recognize a character: " :$$: ShowType ch)

type MakeByte :: Nat -> Nat -> Nat
type family MakeByte x y where
  MakeByte x y = x * 0x10 + y
