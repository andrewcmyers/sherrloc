{-# LANGUAGE TypeFamilies #-}
module Example where

type family F a
type instance F Bool = Int

foo :: F Bool -> Int
foo n = True

-- The only constraint dumped is Bool ~ Int:
-- solveWanteds {
--  WC {wc_flat = [W] cobox_aHH :: Bool ~ Int (CNonCanonical)}
-- Does this example suggests GHC stops immediately when there is a simple error
