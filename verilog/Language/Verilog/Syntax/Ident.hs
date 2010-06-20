--------------------------------------------------------------------------------
-- |
-- Module       :  Language.Verilog.Syntax.Ident
-- Copyright    :  (c) Signali Corp. 2010
-- License      :  All rights reserved
--
-- Maintainer   : pweaver@signalicorp.com
-- Stability    : experimental
-- Portability  : ghc
--
-- Definition of Verilog identifiers.
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Verilog.Syntax.Ident
  ( -- * Identifier
    Ident(..)
  ) where

import Data.Binary      ( Binary )
import Data.Generics    ( Data, Typeable )

--------------------------------------------------------------------------------

-- TODO check if an identifier is valid; convert to valid identifier

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Binary, Data, Typeable)

--------------------------------------------------------------------------------
