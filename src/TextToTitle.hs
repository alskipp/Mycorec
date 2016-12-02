module TextToTitle where

import Data.Char
import Data.Text (Text)
import qualified Data.Text as T hiding (toTitle)
import Data.Text.Internal.Fusion.Common hiding (toTitle)
import Data.Text.Internal.Fusion
import Data.Text.Internal.Fusion.Types
import Data.Text.Internal.Fusion.CaseMapping (lowerMapping, titleMapping)


toTitle :: Text -> Text
toTitle t = unstream (toTitle' (stream t))
{-# INLINE toTitle #-}

toTitle' :: Stream Char -> Stream Char
toTitle' (Stream next0 s0 len) = Stream next (CC (False :*: s0) '\0' '\0') len
  where
    next (CC (letter :*: s) '\0' _) =
      case next0 s of
        Done           -> Done
        Skip s'        -> Skip (CC (letter :*: s') '\0' '\0')
        Yield c s'
          | nonSpace   -> if letter
                          then lowerMapping c (nonSpace :*: s')
                          else titleMapping c (letter' :*: s')
          | otherwise  -> Yield c (CC (letter' :*: s') '\0' '\0')
          where nonSpace = not (isSpace c)
                letter'  = isLetter c
    next (CC s a b)     = Yield a (CC s b '\0')
{-# INLINE [0] toTitle' #-}
