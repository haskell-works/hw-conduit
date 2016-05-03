module HaskellWorks.Data.Conduit.Json.Words where

import           Data.Char
import           Data.Word

wBackslash :: Word8
wBackslash = fromIntegral (ord '\\')

wBackspace :: Word8
wBackspace = fromIntegral (ord '\b')

wDoubleQuote :: Word8
wDoubleQuote = fromIntegral (ord '"')

wUnderscore :: Word8
wUnderscore = fromIntegral (ord '_')

wNewline :: Word8
wNewline = fromIntegral (ord '\n')

wLinefeed :: Word8
wLinefeed = fromIntegral (ord '\f')

wCarriageReturn :: Word8
wCarriageReturn = fromIntegral (ord '\r')

wTab :: Word8
wTab = fromIntegral (ord '\t')

wQuote :: Word8
wQuote = fromIntegral (ord '\'')

wSlash :: Word8
wSlash = fromIntegral (ord '/')

wSpace :: Word8
wSpace = fromIntegral (ord ' ')

wOpenParen :: Word8
wOpenParen = fromIntegral (ord '(')

wCloseParen :: Word8
wCloseParen = fromIntegral (ord ')')

wOpenBracket :: Word8
wOpenBracket = fromIntegral (ord '[')

wCloseBracket :: Word8
wCloseBracket = fromIntegral (ord ']')

wOpenBrace :: Word8
wOpenBrace = fromIntegral (ord '{')

wCloseBrace :: Word8
wCloseBrace = fromIntegral (ord '}')

wPlus :: Word8
wPlus = fromIntegral (ord '+')

wA :: Word8
wA = fromIntegral (ord 'A')

wa :: Word8
wa = fromIntegral (ord 'a')

wB :: Word8
wB = fromIntegral (ord 'B')

wb :: Word8
wb = fromIntegral (ord 'b')

we :: Word8
we = fromIntegral (ord 'e')

wE :: Word8
wE = fromIntegral (ord 'E')

wf :: Word8
wf = fromIntegral (ord 'f')

wn :: Word8
wn = fromIntegral (ord 'n')

wr :: Word8
wr = fromIntegral (ord 'r')

wt :: Word8
wt = fromIntegral (ord 't')

wz :: Word8
wz = fromIntegral (ord 'z')

wZ :: Word8
wZ = fromIntegral (ord 'Z')

wDot :: Word8
wDot = fromIntegral (ord '.')

wMinus :: Word8
wMinus = fromIntegral (ord '-')

w0 :: Word8
w0 = fromIntegral (ord '0')

w1 :: Word8
w1 = fromIntegral (ord '1')

w9 :: Word8
w9 = fromIntegral (ord '9')

w00 :: Word8
w00 = fromIntegral (ord '0')

wFF :: Word8
wFF = fromIntegral (ord '\255')

isLeadingDigit :: Word8 -> Bool
isLeadingDigit w = w == wMinus || (w >= w0 && w <= w9)

isTrailingDigit :: Word8 -> Bool
isTrailingDigit w = w == wPlus || w == wMinus || (w >= w0 && w <= w9) || w == wDot || w == wE || w == we

isAlphabetic :: Word8 -> Bool
isAlphabetic w = (w >= wA && w <= wZ) || (w >= wa && w <= wz)
