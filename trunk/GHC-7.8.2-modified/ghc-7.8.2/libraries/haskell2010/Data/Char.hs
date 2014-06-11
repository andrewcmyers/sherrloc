{-# LANGUAGE CPP, PackageImports #-}
#if __GLASGOW_HASKELL__ >= 701
{-# LANGUAGE Safe #-}
#endif

module Data.Char (
    -- * Characters and strings
      Char

    , String

    -- * Character classification
    -- | Unicode characters are divided into letters, numbers, marks,
    -- punctuation, symbols, separators (including spaces) and others
    -- (including control characters).
    , isControl, isSpace
    , isLower, isUpper, isAlpha, isAlphaNum, isPrint
    , isDigit, isOctDigit, isHexDigit
    , isLetter, isMark, isNumber, isPunctuation, isSymbol, isSeparator

    -- ** Subranges
    , isAscii, isLatin1
    , isAsciiUpper, isAsciiLower

    -- ** Unicode general categories
    , GeneralCategory(..), generalCategory

    -- * Case conversion
    , toUpper, toLower, toTitle  -- :: Char -> Char

    -- * Single digit characters
    , digitToInt        -- :: Char -> Int
    , intToDigit        -- :: Int  -> Char

    -- * Numeric representations
    , ord               -- :: Char -> Int
    , chr               -- :: Int  -> Char

    -- * String representations
    , showLitChar       -- :: Char -> ShowS
    , lexLitChar        -- :: ReadS String
    , readLitChar       -- :: ReadS Char
  ) where
import "base" Data.Char
