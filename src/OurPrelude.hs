{-# LANGUAGE PartialTypeSignatures #-}

module OurPrelude
  ( (>>>)
  , (<|>)
  , (<>)
  , (&)
  , module Control.Error
  , module Control.Monad.Except
  , module Control.Monad.Trans.Class
  , module Control.Monad.IO.Class
  , module Data.Bifunctor
  , Set
  , Text
  , Vector
  , interpolate
  ) where

import Control.Applicative ((<|>))
import Control.Category ((>>>))
import Control.Error
import Control.Monad.Except
import Control.Monad.IO.Class
import Control.Monad.Trans.Class
import Data.Bifunctor
import Data.Function ((&))
import Data.Semigroup ((<>))
import Data.Set (Set)
import Data.Text (Text)
import Data.Vector (Vector)
import Language.Haskell.TH.Quote
import qualified NeatInterpolation

interpolate :: QuasiQuoter
interpolate = NeatInterpolation.text
