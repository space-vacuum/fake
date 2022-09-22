module Fake.Types where

------------------------------------------------------------------------------
import Control.Monad
import System.Random
------------------------------------------------------------------------------

newtype FGen a = MkFGen { unFGen :: StdGen -> a }

instance Functor FGen where
  fmap f (MkFGen h) =
    MkFGen (f . h)

instance Applicative FGen where
  pure x =
    MkFGen (const x)
  (<*>) = ap

instance Monad FGen where

  MkFGen m >>= k =
    MkFGen (\r ->
      let (r1,r2)  = split r
          MkFGen m' = k (m r1)
       in m' r2
    )


------------------------------------------------------------------------------
-- | Run a generator to generate a random value in the IO monad.
generate :: FGen a -> IO a
generate (MkFGen g) = do
    g <$> newStdGen
