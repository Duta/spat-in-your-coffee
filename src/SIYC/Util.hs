module SIYC.Util where

import Control.Monad (join, liftM)
import Data.Traversable (Traversable, mapM)
import Prelude hiding (mapM)

type SourceCode = String
type Identifier = String
type TypeName   = String
type ClassName  = String

mapM' :: (Monad m, Traversable t, Monad t) => (a -> m (t b)) -> t a -> m (t b)
mapM' f = liftM join . mapM f
