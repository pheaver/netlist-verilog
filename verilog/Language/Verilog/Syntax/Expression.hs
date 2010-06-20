--------------------------------------------------------------------------------
-- |
-- Module       :  Language.Verilog.Syntax.Expression
-- Copyright    :  (c) Signali Corp. 2010
-- License      :  All rights reserved
--
-- Maintainer   : pweaver@signalicorp.com
-- Stability    : experimental
-- Portability  : ghc
--
-- Abstract syntax tree definitions for Verilog expressions, operators, and
-- constants.
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, TypeOperators #-}
{-# OPTIONS_DERIVE --append -d Binary #-}

module Language.Verilog.Syntax.Expression
  ( -- * Identifiers
    Ident(..),

    -- * Expressions
    Expression(..), ConstExpr,

    -- * Constants
    Base(..),

    -- * Unary Operators
    UnaryOp(..),

    -- * Binary Operators
    BinaryOp(..),
  ) where

import Data.Generics    ( Data, Typeable )
import Data.Binary      ( Binary(..), putWord8, getWord8 )

import Language.Verilog.Syntax.Ident

--------------------------------------------------------------------------------
-- expressions

-- | Expressions.  We do not yet support literals with @x@ or @z@ in them.
data Expression
  -- TODO: support X, x, z, and Z in numbers
  -- TODO: support fractional numbers (e.g. 3.14)
  -- TODO: support exponent numbers (e.g. 3e10)
  -- | An unsized number
  = ExprNum Integer
  -- | A sized number
  | ExprLit Int Integer
  -- | A variable reference
  | ExprVar Ident
  -- | A literal string, in quotes.  Used for parameter values.
  | ExprString String
  -- | Index operator, e.g. @x[y]@.
  | ExprIndex Ident Expression
  -- | A slice operation of a range of indices, e.g. x[10:15].
  | ExprSlice Ident ConstExpr ConstExpr
  -- these next two aren't in the spec, but they're certainly in the Verilog standard
  | ExprSlicePlus Ident Expression ConstExpr   -- ^ e.g. @x[y +: 10]@
  | ExprSliceMinus Ident Expression ConstExpr  -- ^ e.g. @x[y -: 10]@
  | ExprConcat [Expression]                    -- ^ Concatenation, e.g. @{a, b, c}@
  | ExprMultiConcat Expression [Expression]    -- ^ Replication, e.g. @{10,{a, b, c}}@
  -- TODO: <mintypmax_expression>
  | ExprUnary UnaryOp Expression               -- ^ Application of a unary operator
  | ExprBinary BinaryOp Expression Expression  -- ^ Application of a binary operator
  | ExprCond Expression Expression Expression  -- ^ Conditional expression, e.g. @x ? y : z@
  | ExprFunCall Ident [Expression]             -- ^ Function call, e.g. @f(a, b, c)@
  deriving (Eq, Ord, Show, Data, Typeable)

--------------------------------------------------------------------------------
-- constants

type ConstExpr = Expression

data Base = BinBase | OctBase | DecBase | HexBase
  deriving (Eq, Ord, Show, Data, Typeable)

--------------------------------------------------------------------------------
-- operators

-- | Unary operators.  @Uand@, @UNand@, @UOr@, @UNor@, @UXor@, and @UXnor@ are
-- known as \"reduction operators\".  They work just like Haskell\'s @fold@
-- function.
data UnaryOp
  -- UTilde (~) is bitwise negation, UBang (!) is logical negation
  -- UAnd/UNand/UOr/UNor/UXor/UXnor are sometimes called "reduction operators"
  = UPlus   -- ^ Unary plus operator: @+@
  | UMinus  -- ^ Unary 2\'s complement negation: @-@
  | UBang   -- ^ Logical negation, a.k.a NOT: @!@
  | UTilde  -- ^ Bitwise negation, a.k.a. 1\'s complement: @~@
  | UAnd    -- ^ @AND@ reduction operator: @&@
  | UNand   -- ^ @NAND@ reduction operator: @~&@
  | UOr     -- ^ @OR@ eduction operator: @|@
  | UNor    -- ^ @NOR@ reduction operator: @~|@
  | UXor    -- ^ @XOR@ reduction operator: @^@
  | UXnor   -- ^ @XNOR@ reduction operator: @^~@ or @~^@
  deriving (Eq, Ord, Data, Typeable)

instance Show UnaryOp where
  show UPlus  = "+"
  show UMinus = "-"
  show UBang  = "!"
  show UTilde = "~"
  show UAnd   = "&"
  show UNand  = "~&"
  show UOr    = "|"
  show UNor   = "~|"
  show UXor   = "^"
  show UXnor  = "^~" -- "~^" is also valid

-- | Binary operators.
data BinaryOp
  = Pow          -- ^ Arithmetic exponentiation: @**@.  Introduced in Verilog-2001.
  | Plus         -- ^ Arithmetic addition: @+@.
  | Minus        -- ^ Arithmetic subtraction: @-@
  | Times        -- ^ Arithmetic multiplication: @*@
  | Divide       -- ^ Arithmetic division: @/@
  | Modulo       -- ^ Arithmetic modulo: @%@
  | Equals       -- ^ Logical equality: @==@
  | NotEquals    -- ^ Logical inequality: @!=@
  | CEquals      -- ^ Case equality: @===@.  4-state logic, where @x@ and @z@ are
                 -- taken literally.
  | CNotEquals   -- ^ Case inequality: @!==@. 4-state logic, where @x@ and @z@
                 -- are taken literally.
  | LAnd         -- ^ Logical @AND@ operation: @&&@
  | LOr          -- ^ Logical @OR@ operation: @||@
  | LessThan     -- ^ Less than: @<@
  | LessEqual    -- ^ Less than or equal to: @<=@
  | GreaterThan  -- ^ Greater than: @>@
  | GreaterEqual -- ^ Greater than or equal to: @>=@
  | And          -- ^ Bitwise @AND@ operation: @&@
  | Nand         -- ^ Bitwise @NAND@ operation: @~&@
  | Or           -- ^ Bitwise @OR@ operation: @|@
  | Nor          -- ^ Bitwise @NOR@ operation: @~|@
  | Xor          -- ^ Bitwise @XOR@ operation: @^@
  | Xnor         -- ^ Bitwise @XNOR@ operation: @^~@ or @~^@
  | ShiftLeft    -- ^ Logical left shift: @<<@
  | ShiftRight   -- ^ Logical right shift: @>>@
  deriving (Eq, Ord, Data, Typeable)

instance Show BinaryOp where
  show Pow          = "**"
  show Plus         = "+"
  show Minus        = "-"
  show Times        = "*"
  show Divide       = "/"
  show Modulo       = "%"
  show Equals       = "=="
  show NotEquals    = "!="
  show CEquals      = "==="
  show CNotEquals   = "!=="
  show LAnd         = "&&"
  show LOr          = "||"
  show LessThan     = "<"
  show LessEqual    = "<="
  show GreaterThan  = ">"
  show GreaterEqual = ">="
  show And          = "&"
  show Nand         = "~&"
  show Or           = "|"
  show Nor          = "~|"
  show Xor          = "^"
  show Xnor         = "^~"
  show ShiftLeft    = "<<"
  show ShiftRight   = ">>"

--------------------------------------------------------------------------------
-- GENERATED START


instance Binary Expression where
        put x
          = case x of
                ExprNum x1 -> do putWord8 0
                                 put x1
                ExprLit x1 x2 -> do putWord8 1
                                    put x1
                                    put x2
                ExprVar x1 -> do putWord8 2
                                 put x1
                ExprString x1 -> do putWord8 3
                                    put x1
                ExprIndex x1 x2 -> do putWord8 4
                                      put x1
                                      put x2
                ExprSlice x1 x2 x3 -> do putWord8 5
                                         put x1
                                         put x2
                                         put x3
                ExprSlicePlus x1 x2 x3 -> do putWord8 6
                                             put x1
                                             put x2
                                             put x3
                ExprSliceMinus x1 x2 x3 -> do putWord8 7
                                              put x1
                                              put x2
                                              put x3
                ExprConcat x1 -> do putWord8 8
                                    put x1
                ExprMultiConcat x1 x2 -> do putWord8 9
                                            put x1
                                            put x2
                ExprUnary x1 x2 -> do putWord8 10
                                      put x1
                                      put x2
                ExprBinary x1 x2 x3 -> do putWord8 11
                                          put x1
                                          put x2
                                          put x3
                ExprCond x1 x2 x3 -> do putWord8 12
                                        put x1
                                        put x2
                                        put x3
                ExprFunCall x1 x2 -> do putWord8 13
                                        put x1
                                        put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (ExprNum x1)
                   1 -> do x1 <- get
                           x2 <- get
                           return (ExprLit x1 x2)
                   2 -> do x1 <- get
                           return (ExprVar x1)
                   3 -> do x1 <- get
                           return (ExprString x1)
                   4 -> do x1 <- get
                           x2 <- get
                           return (ExprIndex x1 x2)
                   5 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (ExprSlice x1 x2 x3)
                   6 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (ExprSlicePlus x1 x2 x3)
                   7 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (ExprSliceMinus x1 x2 x3)
                   8 -> do x1 <- get
                           return (ExprConcat x1)
                   9 -> do x1 <- get
                           x2 <- get
                           return (ExprMultiConcat x1 x2)
                   10 -> do x1 <- get
                            x2 <- get
                            return (ExprUnary x1 x2)
                   11 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (ExprBinary x1 x2 x3)
                   12 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (ExprCond x1 x2 x3)
                   13 -> do x1 <- get
                            x2 <- get
                            return (ExprFunCall x1 x2)
                   _ -> error "Corrupted binary data for Expression"


instance Binary Base where
        put x
          = case x of
                BinBase -> putWord8 0
                OctBase -> putWord8 1
                DecBase -> putWord8 2
                HexBase -> putWord8 3
        get
          = do i <- getWord8
               case i of
                   0 -> return BinBase
                   1 -> return OctBase
                   2 -> return DecBase
                   3 -> return HexBase
                   _ -> error "Corrupted binary data for Base"


instance Binary UnaryOp where
        put x
          = case x of
                UPlus -> putWord8 0
                UMinus -> putWord8 1
                UBang -> putWord8 2
                UTilde -> putWord8 3
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
                   2 -> return UBang
                   3 -> return UTilde
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
                   _ -> error "Corrupted binary data for BinaryOp"
-- GENERATED STOP
