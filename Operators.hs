-- -----------------------------------------------------------------------------
-- Copyright (c) 2010 Signali Corp.
--
-- some common HDL prefix and infix operators
--
-- TODO:
--  * find the appropriate namespace for this module (Language.HDL?)
--  * haddock-ify
--
-- Notes
--  * precedence and pretty-printing are language specific, and defined elsewhere.
--  * exponentation and rotate operators were introduced in Verilog-2001
--  * some operators are not prefix/infix, such as verilog concatenation and the
--    conditional (?) operator.  those operators are defined elsewhere.
--  * VHDL has both "logical" and "arithmetic" shift operators, which we don't yet support here.
--  * VHDL has both a 'mod' and a 'rem' operator, but so far we only define Modulo.
--  * VHDL has a concat operator (&) that isn't yet supported here.
--  * VHDL has 'abs' operator that isn't yet supported here.
-- -----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_DERIVE --append -d Binary #-}

module Operators where

import Data.Binary      ( Binary(..), putWord8, getWord8 )
import Data.Generics	(Data, Typeable)

-- -----------------------------------------------------------------------------

data UnaryOp
  -- LNeg is logical negation, Neg is bitwise negation
  -- UAnd/UNand/UOr/UNor/UXor/UXnor are sometimes called "reduction operators"
  = UPlus | UMinus | LNeg | Neg | UAnd | UNand | UOr | UNor | UXor | UXnor
  deriving (Eq, Ord, Show, Data, Typeable)

data BinaryOp
  = Pow | Plus | Minus | Times | Divide | Modulo      -- arithmetic
  | Equals | NotEquals                                -- logical equality
  | CEquals | CNotEquals                              -- case equality
  | LAnd | LOr                                        -- logical and/or
  | LessThan | LessEqual | GreaterThan | GreaterEqual -- relational
  | And | Nand | Or | Nor | Xor | Xnor                -- bitwise
  | ShiftLeft | ShiftRight | RotateLeft | RotateRight -- shift/rotate
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- GENERATED START

 
instance Binary UnaryOp where
        put x
          = case x of
                UPlus -> putWord8 0
                UMinus -> putWord8 1
                LNeg -> putWord8 2
                Neg -> putWord8 3
                UAnd -> putWord8 4
                UNand -> putWord8 5
                UOr -> putWord8 6
                UNor -> putWord8 7
                UXor -> putWord8 8
                UXnor -> putWord8 9
        get
          = do i <- getWord8
               case i of
                   0 -> return UPlus
                   1 -> return UMinus
                   2 -> return LNeg
                   3 -> return Neg
                   4 -> return UAnd
                   5 -> return UNand
                   6 -> return UOr
                   7 -> return UNor
                   8 -> return UXor
                   9 -> return UXnor
                   _ -> error "Corrupted binary data for UnaryOp"

 
instance Binary BinaryOp where
        put x
          = case x of
                Pow -> putWord8 0
                Plus -> putWord8 1
                Minus -> putWord8 2
                Times -> putWord8 3
                Divide -> putWord8 4
                Modulo -> putWord8 5
                Equals -> putWord8 6
                NotEquals -> putWord8 7
                CEquals -> putWord8 8
                CNotEquals -> putWord8 9
                LAnd -> putWord8 10
                LOr -> putWord8 11
                LessThan -> putWord8 12
                LessEqual -> putWord8 13
                GreaterThan -> putWord8 14
                GreaterEqual -> putWord8 15
                And -> putWord8 16
                Nand -> putWord8 17
                Or -> putWord8 18
                Nor -> putWord8 19
                Xor -> putWord8 20
                Xnor -> putWord8 21
                ShiftLeft -> putWord8 22
                ShiftRight -> putWord8 23
                RotateLeft -> putWord8 24
                RotateRight -> putWord8 25
        get
          = do i <- getWord8
               case i of
                   0 -> return Pow
                   1 -> return Plus
                   2 -> return Minus
                   3 -> return Times
                   4 -> return Divide
                   5 -> return Modulo
                   6 -> return Equals
                   7 -> return NotEquals
                   8 -> return CEquals
                   9 -> return CNotEquals
                   10 -> return LAnd
                   11 -> return LOr
                   12 -> return LessThan
                   13 -> return LessEqual
                   14 -> return GreaterThan
                   15 -> return GreaterEqual
                   16 -> return And
                   17 -> return Nand
                   18 -> return Or
                   19 -> return Nor
                   20 -> return Xor
                   21 -> return Xnor
                   22 -> return ShiftLeft
                   23 -> return ShiftRight
                   24 -> return RotateLeft
                   25 -> return RotateRight
                   _ -> error "Corrupted binary data for BinaryOp"
-- GENERATED STOP
