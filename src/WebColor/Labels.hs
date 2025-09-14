{-# LANGUAGE DataKinds #-}
{-# LANGUAGE NoStarIsType #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE AllowAmbiguousTypes #-}

-- We need this to have correct links inside haddocks
{-# OPTIONS_GHC -Wno-unused-imports #-}

-- Required to use `showType` inside haddock coments
{-# OPTIONS_GHC -Wno-unused-top-binds #-}

{- |
= Introduction and intended usage

This library contains a small number of helpers that primarily aim
to help users construct `IsLabel` instances to use @-XOverloadedLabels@
syntax to construct their color types using widely known web color syntax.

There are two ways to use this API:

1. Simple type-class-based API

2. Advanced manual type-family-based API

You should always prefer the simple API.

== Simple API

You should use the simple API to write an `IsLabel` instance for your own
type representing a color.

Depending on whether your type supports representing the alpha channel, you should
choose between `IsWebColor` and `IsWebColorAlpha` type classes.

Therefore, it's likely that you want to write an instance that looks like this:

@
data RGBA = MkRGBA { red :: `Word8`, green :: `Word8`, blue :: `Word8`, alpha :: `Word8` }

instance `IsWebColorAlpha` s => `IsLabel` s RGBA where
  `fromLabel` = `webColorAlpha` @s \r g b a ->
      MkRGBA { red = r, green = g, blue = b, alpha = a }
@

Or this in case your type doesn't have an alpha channel:

@
data RGB = MkRGB { red :: `Word8`, green :: `Word8`, blue :: `Word8` }

instance `IsWebColor` s => `IsLabel` s RGB where
  `fromLabel` = `webColor` @s \r g b ->
      MkRGB { red = r, green = g, blue = b }
@

Please do not try to use `IsWebColor` if your type can represent either
RGB or RGBA colors; use `IsWebColorAlpha` and alternate data construction
depending on the value of the alpha channel:

@
data Color
  = MkRGB { red :: `Word8`, green :: `Word8`, blue :: `Word8` }
  | MkRGBA { red :: `Word8`, green :: `Word8`, blue :: `Word8`, alpha :: `Word8` }

instance `IsWebColorAlpha` s => `IsLabel` s RGBA where
  `fromLabel` = `webColorAlpha` @s \r g b a ->
      if a `==` 255
        then MkRGB { red = r, green = g, blue = b }
        else MkRGBA { red = r, green = g, blue = b, alpha = a }
@

== Advanced API

It's always recommended to use the simple API unless you want to drop
named colors for some reason or want to work with raw `KnownNat`s.

There are four main type families:

* `ParseWebColor`
* `ParseWebColorAlpha`
* `ParseWebColorMaybeAlpha`
* `ParseHexadecimalColor`

The first two correspond to the output of `IsWebColor` and `IsWebColorAlpha` type
classes listed in the previous section.

`ParseWebColorMaybeAlpha` allows you to alternate behavior between presence and
absence of the alpha channel in the input. Please let me know if you have a use case
for this and why you can't simply consider 255 alpha as an RGB color.

`ParseHexadecimalColor` converts hexadecimal strings of length 3, 4, 6, and 8 into
correct color channels but doesn't accept named colors.

In general, you give a parsing function a symbol argument and match results using the equality
operator:

@
f :: `ParseWebColorAlpha` s ~ '(r,g,b,a) => ...
@

After that, you can set various constraints on the results. For example, this is how you can
convert given values into a 32-bit number without any runtime computations:

@
{-# LANGUAGE DataKinds, NoStarIsType, AllowAmbiguousTypes #-}

f ::  forall s r g b a w32.
      `ParseWebColorAlpha` s ~ '(r,g,b,a) =>
      ((r * 0x100 + g) * 0x100 + b) * 0x100 + a ~ w32 =>
      `KnownNat` w32 => `Word32`
f = `fromInteger` `$` `natVal` (`Proxy` @w32)
@

== To advanced end users

If you're an advanced Haskell user. If you think that rules are just "recommendations."
If you're ready to face possible breakages just for slight usability improvements.
Then I present you a footgun.

It is possible to write orphan instances for `IsWebColor` and `IsWebColorAlpha` type classes
to extend #-syntax with custom named colors, just like this:

@
instance `IsWebColor` "gold" where
  `webColor` k = k 255 215 0

instance `IsWebColorAlpha` "gold" where
  `webColorAlpha` k = k 255 215 0 255
@

I'm not going to guarantee you anything. Enjoy!
-}

module WebColor.Labels
  ( -- * Primary API
    IsWebColor(..),
    IsWebColorAlpha(..),
    -- * Advanced type-level API
    WebColor,
    WebColorAlpha,
    ParseWebColor,
    ParseWebColorAlpha,
    WebColorParsed,
    ParseWebColorMaybeAlpha,
    ParseHexadecimalColor,
    -- * Utility type families
    --
    -- | These type families aren't required to use
    -- the library, but may be useful for defining your
    -- own parse routing on top of web colors
    ParseHexadecimalChar,
  ) where

import Data.Proxy (Proxy (..))
#if __GLASGOW_HASKELL__ >= 906
import Data.Type.Equality (type (~))
#endif
import Data.Word (Word8, Word32)
import GHC.TypeLits
  (ErrorMessage (..), KnownNat, Nat, Symbol, TypeError, UnconsSymbol, natVal, type (*), type (+))
import Prelude (Char, Maybe (..), fromInteger, ($!), ($), (==))
import Data.Kind (Constraint)

-- Imports for haddocks
import GHC.OverloadedLabels (IsLabel(..))

#if __GLASGOW_HASKELL__ >= 908
import GHC.TypeError (Unsatisfiable)

showType :: forall a. (Unsatisfiable (ShowType a)) => ()
showType = ()
#endif

-- | Red, green, blue, and optional alpha color channels in range from 0 to 255
type WebColorParsed = (Nat, Nat, Nat, Maybe Nat)

-- | Parse a string containing a named color from a basic
-- palette or hexadecimal representation with an optional
-- alpha channel.
--
-- * Named colors are always considered RGB-only:
--
-- >>> showType @(`ParseWebColorMaybeAlpha` "red")
-- '(255, 0, 0, 'Nothing)
--
-- >>> showType @(`ParseWebColorMaybeAlpha` "silver")
-- '(192, 192, 192, 'Nothing)
--
-- `ParseWebColorMaybeAlpha` inherits many properties from `ParseHexadecimalColor`:
--
-- * The difference between a `Nothing` alpha channel and @`Just` 255@
--   is purely syntactical, and you have to tackle the difference yourself
-- * This family automatically handles shortened syntax
-- * This family throws a compile-time error when the wrong number
--   of hex characters is supplied
--
-- Please refer to `ParseHexadecimalColor` for the examples.
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
  ParseWebColorMaybeAlpha s = ParseHexadecimalColor s

-- | Parse a string containing hexadecimal representation of a
-- color with an optional alpha channel.
--
-- * This family doesn't handle named colors.
-- * The difference between a `Nothing` alpha channel and @`Just` 255@
--   is purely syntactical, and you have to tackle the difference yourself:
--
-- >>> showType @(`ParseHexadecimalColor` "fafefb")
-- '(250, 254, 251, 'Nothing)
--
-- >>> showType @(`ParseHexadecimalColor` "fafefbff")
-- '(250, 254, 251, 'Just 255)
--
-- * This family automatically handles shortened syntax:
--
-- E.g. #123 is the same as #112233:
--
-- >>> showType @(`ParseHexadecimalColor` "123")
-- '(17, 34, 51, 'Nothing)
--
-- >>> showType @(`ParseHexadecimalColor` "112233")
-- '(17, 34, 51, 'Nothing)
--
-- The same applies to #1234 and #11223344:
--
-- >>> showType @(`ParseHexadecimalColor` "1234")
-- '(17, 34, 51, 'Just 68)
--
-- >>> showType @(`ParseHexadecimalColor` "11223344")
-- '(17, 34, 51, 'Just 68)
--
-- * This family throws a compile-time error when the wrong number
-- of hex characters is supplied:
--
-- >>> showType @(`ParseHexadecimalColor` "1")
-- Unexpected number of hex codes
-- expected 3, 4, 6 or 8
--
-- >>> showType @(`ParseHexadecimalColor` "1122334455")
-- Unexpected number of hex codes
-- expected 3, 4, 6 or 8
type ParseHexadecimalColor :: Symbol -> WebColorParsed
type family ParseHexadecimalColor s where
  ParseHexadecimalColor s = ParseColorRec '[] (UnconsSymbol s)

type ParseColorRec :: [Nat] -> Maybe (Char, Symbol) -> WebColorParsed
type family ParseColorRec color str where
  ParseColorRec colors Nothing = UpgradeColor colors
  ParseColorRec colors (Just '(ch, t)) = ParseColorRec (ParseHexadecimalChar ch : colors) (UnconsSymbol t)

-- | Red, green, and blue color channels in range from 0 to 255
type WebColor = (Nat, Nat, Nat)

-- | Parse a string containing a named color from a basic
-- palette or hexadecimal representation without an alpha channel.
--
-- * For convenience, a 255 alpha channel is interpreted
--   as a solid color without an alpha channel:
--
-- >>> showType @(`ParseWebColor` "123")
-- '(17, 34, 51)
--
-- >>> showType @(`ParseWebColor` "123f")
-- '(17, 34, 51)
--
-- * Other alpha channel values result in a compile-time error:
--
-- >>> showType @(`ParseWebColor` "123d")
-- Unexpected alpha channel! RGB color expected
--
-- This family inherits many properties from `ParseWebColorMaybeAlpha`:
--
-- * Named colors are always considered RGB-only:
--
-- >>> showType @(`ParseWebColor` "red")
-- '(255, 0, 0)
--
-- * This family automatically handles shortened syntax.
-- * This family throws a compile-time error when the wrong number
--   of hex characters is supplied
type ParseWebColor :: Symbol -> WebColor
type family ParseWebColor s where
  ParseWebColor s = ParseRGBColorWorker (ParseWebColorMaybeAlpha s)

type ParseRGBColorWorker :: WebColorParsed -> WebColor
type family ParseRGBColorWorker color where
  ParseRGBColorWorker '(r, g, b, Nothing)  = '(r, g, b)
  ParseRGBColorWorker '(r, g, b, Just 255) = '(r, g, b)
  ParseRGBColorWorker '(_, _, _, Just _) = TypeError (Text "Unexpected alpha channel! RGB color expected")

-- | Red, green, blue, and alpha color channels in range from 0 to 255
type WebColorAlpha = (Nat, Nat, Nat, Nat)

-- | Parse a string containing a named color from a basic
-- palette or a hexadecimal representation with an optional
-- alpha channel.
--
-- * This family converts an absence of an alpha channel into
--   the 255 value.
--
-- >>> showType @(`ParseWebColorAlpha` "123")
-- '(17, 34, 51, 255)
--
-- This family inherits many properties from `ParseWebColorMaybeAlpha`:
--
-- * Named colors are always considered RGB-only and are handled the same way:
--
-- >>> showType @(`ParseWebColorAlpha` "red")
-- '(255, 0, 0, 255)
--
-- * This family automatically handles shortened syntax.
-- * This family throws a compile-time error when the wrong number
--   of hex characters is supplied
type ParseWebColorAlpha :: Symbol -> WebColorAlpha
type family ParseWebColorAlpha s where
  ParseWebColorAlpha s = ParseRGBAColorWorker (ParseWebColorMaybeAlpha s)

type ParseRGBAColorWorker :: WebColorParsed -> WebColorAlpha
type family ParseRGBAColorWorker color where
  ParseRGBAColorWorker '(r, g, b, Nothing) = '(r, g, b, 0xFF)
  ParseRGBAColorWorker '(r, g, b, Just a)  = '(r, g, b, a)

type a & b = a

-- | Parse a type-level string containing a named color from a basic
-- palette or a hexadecimal representation and convert it into three
-- bytes representing red, green, and blue channels
--
--
-- * For convenience, a 255 alpha channel is interpreted
--   as a solid color without an alpha channel:
--
-- >>> webColor @"123" (,,)
-- (17,34,51)
--
-- >>> webColor @"123f" (,,)
-- (17,34,51)
--
-- * Other alpha channel values result in a compile-time error:
--
-- >>> webColor @"123d" (,,)
-- Unexpected alpha channel! RGB color expected
--
-- * Named colors are always considered RGB-only:
--
-- >>> webColor @"red" (,,)
-- (255,0,0)
type IsWebColor :: Symbol -> Constraint
class IsWebColor s where
  -- | Parse a type-level string and give you bytes representing
  -- red, green, and blue colors.
  --
  -- NB: @&@ is just a type alias that should help to keep with order.
  webColor ::
    (Word8 & "red" -> Word8 & "green" -> Word8 & "blue" -> r) -> r

-- | Hacky instance to avoid a warning from GHC
instance {-# OVERLAPPING #-} IsWebColor "red" where
  webColor k = k 255 0 0

-- | Main instance where all the magic happens
instance {-# OVERLAPPABLE #-}
  ( ParseWebColor s ~ '(r, g, b),
    KnownNat r, KnownNat g, KnownNat b
  ) => IsWebColor s where
  webColor k =
      k `color` Proxy @r `color` Proxy @g `color` Proxy @b

-- | Parse a type-level string containing a named color from a basic
-- palette or a hexadecimal representation and convert it into four
-- bytes representing red, green, blue, and alpha channels
--
--
-- * An absent alpha channel  get converted into the 255 value:
--
-- >>> webColorAlpha @"123" (,,,)
-- (17,34,51,255)
--
-- >>> webColorAlpha @"123f" (,,,)
-- (17,34,51,255)
--
-- * Named colors are always considered RGB-only:
--
-- >>> webColorAlpha @"red" (,,,)
-- (255,0,0,255)
type IsWebColorAlpha :: Symbol -> Constraint
class IsWebColorAlpha s where
  -- | Parse a type-level string and give you bytes representing
  -- red, green, blue, and alpha colors.
  --
  -- NB: @&@ is just a type alias that should help to keep with order.
  webColorAlpha ::
    (Word8 & "red" -> Word8 & "green" -> Word8 & "blue" -> Word8 & "alpha" -> r) -> r

-- | Hacky instance to avoid a warning from GHC
instance {-# OVERLAPPING #-} IsWebColorAlpha "red" where
  webColorAlpha k = k 255 0 0 255

-- | Main instance where all the magic happens
instance {-# OVERLAPPABLE #-}
  ( ParseWebColorAlpha s ~ '(r, g, b, a),
    KnownNat r, KnownNat g, KnownNat b, KnownNat a
  ) => IsWebColorAlpha s where
  webColorAlpha k =
      k `color` Proxy @r `color` Proxy @g `color` Proxy @b `color` Proxy @a

color :: KnownNat n => (Word8 -> r) -> Proxy n -> r
color k p = k $! fromInteger (natVal p)

type UpgradeColor :: [Nat] -> WebColorParsed
type family UpgradeColor nats where
  UpgradeColor [        b,      g,      r     ] = UpgradeColor [      b,  b,  g,  g,  r,  r]
  UpgradeColor [a,      b,      g,      r     ] = UpgradeColor [a, a, b,  b,  g,  g,  r,  r]
  UpgradeColor [        b1, b2, g1, g2, r1, r2] = '(MakeByte r2 r1, MakeByte g2 g1, MakeByte b2 b1, Nothing)
  UpgradeColor [a1, a2, b1, b2, g1, g2, r1, r2] = '(MakeByte r2 r1, MakeByte g2 g1, MakeByte b2 b1, Just (MakeByte a2 a1))
  UpgradeColor _ = TypeError (Text "Unexpected number of hex codes" :$$: Text "expected 3, 4, 6 or 8")

-- | Convert a hexadecimal character (0 .. F) into a corresponging
-- number. Case insensitive.
type ParseHexadecimalChar :: Char -> Nat
type family ParseHexadecimalChar ch where
  ParseHexadecimalChar '0' = 0x0
  ParseHexadecimalChar '1' = 0x1
  ParseHexadecimalChar '2' = 0x2
  ParseHexadecimalChar '3' = 0x3
  ParseHexadecimalChar '4' = 0x4
  ParseHexadecimalChar '5' = 0x5
  ParseHexadecimalChar '6' = 0x6
  ParseHexadecimalChar '7' = 0x7
  ParseHexadecimalChar '8' = 0x8
  ParseHexadecimalChar '9' = 0x9
  ParseHexadecimalChar 'a' = 0xa
  ParseHexadecimalChar 'b' = 0xb
  ParseHexadecimalChar 'c' = 0xc
  ParseHexadecimalChar 'd' = 0xd
  ParseHexadecimalChar 'e' = 0xe
  ParseHexadecimalChar 'f' = 0xf
  ParseHexadecimalChar 'A' = 0xA
  ParseHexadecimalChar 'B' = 0xB
  ParseHexadecimalChar 'C' = 0xC
  ParseHexadecimalChar 'D' = 0xD
  ParseHexadecimalChar 'E' = 0xE
  ParseHexadecimalChar 'F' = 0xF
  ParseHexadecimalChar ch  = TypeError (Text "Unable to recognize a character: " :$$: ShowType ch)

type MakeByte :: Nat -> Nat -> Nat
type family MakeByte x y where
  MakeByte x y = x * 0x10 + y
