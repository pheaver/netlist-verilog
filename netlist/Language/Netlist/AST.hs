-- -----------------------------------------------------------------------------
-- Copyright (c) 2010 Signali Corp.
--
-- An abstract syntax tree (AST) for a generic netlist, somewhere between SPIR
-- and HDLs like Verilog and VHDL.
--
-- there are no definitive semantics assigned to this AST.
--
-- for example, the user may choose to treat the bindings as recursive, so that
-- expressions can reference variables before their declaration, like in
-- Haskell, which is not supported in Verilog and VHDL.  in this case, the user
-- must fix the bindings when converting to an HDL.
--
-- also, the user may treat module instantiations and processes as having an
-- implict clock/reset, so that they are not explicitly named in those
-- constructs in this AST.  then, any "Always" triggers and module
-- instantiations can be interpreted as having an implicit clock/reset, which
-- are inserted whe generating HDL.
--
-- when you instantiate a module but information about that module is missing
-- (e.g. the clock/reset are implicit and you need to know what they are called
-- in that module), you can use ExternDecl (TODO) to declare a module's
-- interface so that you know how to instantiate it, or retrieve the interface
-- from a user-maintained database or by parsing and extracting from an HDL
-- file.
-- -----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable #-}
{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_DERIVE --append -d Binary #-}

module Language.Netlist.AST where

import Data.Binary      ( Binary(..), putWord8, getWord8 )
import Data.Generics	( Data, Typeable )

-- -----------------------------------------------------------------------------

-- corresponds to a Verilog "module" or VHDL "entity"
data Module
  = Module { module_name    :: Ident
           , module_inputs  :: [(Ident, Maybe Range)]
           , module_outputs :: [(Ident, Maybe Range)]
           , module_decls   :: [Decl]
           -- TODO: support static parameters (VHDL "generic", Verilog "parameter")
           }
  deriving (Eq, Ord, Show, Data, Typeable)

-- identifier name.  we could make this something like SPIR.Var, and/or
-- parameterize the AST over it.
type Ident = String

-- size of a wire/mem
type Size = Int

-- a declaration -- analogous to `module_item' in Verilog formal syntax.
data Decl
  -- A net (aka 'wire') has a continuously assigned value.  The net can be
  -- declared and assigned at the same time (NetDecl with Just Expr), or
  -- separately (NetDecl with Nothing, and a separate NetAssign).
  = NetDecl Ident (Maybe Range) (Maybe Expr)
  | NetAssign Ident Expr

  -- A mem (aka 'reg') is stateful.  it can be assigned by a non-blocking
  -- assignment (or blocking, but we don't support those yet) within a process.
  -- TODO: support optional initial value

  -- The first range is the most significant dimension.
  -- So, MemDecl x (0, 31) (7, 0) corresponds to the following in Verilog:
  --    reg [7:0] x [0:31]

  | MemDecl Ident (Maybe Range) (Maybe Range)

  -- a module/entity instantiation
  | InstDecl Ident            -- name of the module
             Ident            -- name of the instance
             [(Ident, Expr)]  -- parameter assignments
             [(Ident, Expr)]  -- input port connections
             [(Ident, Expr)]  -- output port connections

  -- declare an external module entity
  -- TODO: ExternDecl ExternLang

  -- Below is a general process construct, compatible with both VHDL and Verilog
  -- processes.  It supports positive and negative edge triggers and a body (a
  -- statement) for each trigger.

  -- Here are loose semantics of a process [(trigger0, stmt0), (trigger1, stmt1)...]:
  --
  -- if trigger0
  --    statement0
  -- else if
  --    trigger1
  -- ...

  | ProcessDecl [(Event, Stmt)]

  -- execute once at the beginning of simulation
  | InitProcessDecl Stmt

  deriving (Eq, Ord, Show, Data, Typeable)

data Range
  = Range ConstExpr ConstExpr
  deriving (Eq, Ord, Show, Data, Typeable)

type ConstExpr = Expr

data Event
  = Event Expr Edge
  deriving (Eq, Ord, Show, Data, Typeable)

data Edge
  = PosEdge
  | NegEdge
  -- TODO: AnyEdge?
  deriving (Eq, Ord, Show, Data, Typeable)

-- Expr is combination of VHDL and Verilog expressions.

-- In VHDL, concatenation is a binary operator, but in Verilog it takes any
-- number of arguments.  In this AST, we define it like the Verilog operator.
-- If we translate to VHDL, we have to convert it to the VHDL binary operator.

-- There are some HDL operators that we don't represent here.  For example, in
-- Verilog there is a multiple concatenation (a.k.a. replication) operator,
-- which we don't bother to support.

data Expr
  -- terminal nodes
  = ExprNum Integer               -- un unsized number
  | ExprLit Size Integer          -- a sized bitvector
  | ExprBit Int                   -- in vhdl, bits are different than 1-bit bitvectors
  | ExprVar Ident
  | ExprString String             -- a quoted string (useful for parameters)
  -- recursive nodes
  | ExprIndex Ident Expr          -- x[e]
  | ExprSlice Ident Expr Expr     -- x[e1 : e2]
  | ExprSliceOff Ident Expr Int   -- x[e : e+i] -- 'i' can be negative
  | ExprConcat [Expr]
  | ExprCond Expr Expr Expr
  | ExprUnary UnaryOp Expr
  | ExprBinary BinaryOp Expr Expr
  | ExprFunCall Ident [Expr]      -- function application
  deriving (Eq, Ord, Show, Data, Typeable)

-- behavioral statement
data Stmt
  = Assign LValue Expr         -- non-blocking assignment
  | If Expr Stmt (Maybe Stmt)  -- if statement
  | Case Expr [([Expr], Stmt)] (Maybe Stmt)
                               -- case statement, with optional default case
  | Seq [Stmt]                 -- multiple statements (between 'begin' and 'end')
  | FunCallStmt Ident [Expr]   -- a function call that can appear as a statement,
                               -- useful for calling Verilog tasks (e.g. $readmem).
  deriving (Eq, Ord, Show, Data, Typeable)

-- not all expressions are allowed on the LHS of an assignment, but we're lazy
-- and don't enforce that restriction in the AST.
type LValue = Expr

-- These operators include almost all VHDL and Verilog operators.
-- Notes
--  * precedence and pretty-printing are language specific, and defined elsewhere.
--  * exponentation and rotate operators were introduced in Verilog-2001
--  * some operators are not prefix/infix, such as verilog concatenation and the
--    conditional (?) operator.  those operators are defined elsewhere.
--  * VHDL has both "logical" and "arithmetic" shift operators, which we don't yet support here.
--  * VHDL has both a 'mod' and a 'rem' operator, but so far we only define Modulo.
--  * VHDL has a concat operator (&) that isn't yet supported here.
--  * VHDL has 'abs' operator that isn't yet supported here.

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


instance Binary Module where
        put (Module x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (Module x1 x2 x3 x4)


instance Binary Decl where
        put x
          = case x of
                NetDecl x1 x2 x3 -> do putWord8 0
                                       put x1
                                       put x2
                                       put x3
                NetAssign x1 x2 -> do putWord8 1
                                      put x1
                                      put x2
                MemDecl x1 x2 x3 -> do putWord8 2
                                       put x1
                                       put x2
                                       put x3
                InstDecl x1 x2 x3 x4 x5 -> do putWord8 3
                                              put x1
                                              put x2
                                              put x3
                                              put x4
                                              put x5
                ProcessDecl x1 -> do putWord8 4
                                     put x1
                InitProcessDecl x1 -> do putWord8 5
                                         put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (NetDecl x1 x2 x3)
                   1 -> do x1 <- get
                           x2 <- get
                           return (NetAssign x1 x2)
                   2 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (MemDecl x1 x2 x3)
                   3 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           x5 <- get
                           return (InstDecl x1 x2 x3 x4 x5)
                   4 -> do x1 <- get
                           return (ProcessDecl x1)
                   5 -> do x1 <- get
                           return (InitProcessDecl x1)
                   _ -> error "Corrupted binary data for Decl"


instance Binary Range where
        put (Range x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Range x1 x2)


instance Binary Event where
        put (Event x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Event x1 x2)


instance Binary Edge where
        put x
          = case x of
                PosEdge -> putWord8 0
                NegEdge -> putWord8 1
        get
          = do i <- getWord8
               case i of
                   0 -> return PosEdge
                   1 -> return NegEdge
                   _ -> error "Corrupted binary data for Edge"


instance Binary Expr where
        put x
          = case x of
                ExprNum x1 -> do putWord8 0
                                 put x1
                ExprLit x1 x2 -> do putWord8 1
                                    put x1
                                    put x2
                ExprBit x1 -> do putWord8 2
                                 put x1
                ExprVar x1 -> do putWord8 3
                                 put x1
                ExprString x1 -> do putWord8 4
                                    put x1
                ExprIndex x1 x2 -> do putWord8 5
                                      put x1
                                      put x2
                ExprSlice x1 x2 x3 -> do putWord8 6
                                         put x1
                                         put x2
                                         put x3
                ExprSliceOff x1 x2 x3 -> do putWord8 7
                                            put x1
                                            put x2
                                            put x3
                ExprConcat x1 -> do putWord8 8
                                    put x1
                ExprCond x1 x2 x3 -> do putWord8 9
                                        put x1
                                        put x2
                                        put x3
                ExprUnary x1 x2 -> do putWord8 10
                                      put x1
                                      put x2
                ExprBinary x1 x2 x3 -> do putWord8 11
                                          put x1
                                          put x2
                                          put x3
                ExprFunCall x1 x2 -> do putWord8 12
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
                           return (ExprBit x1)
                   3 -> do x1 <- get
                           return (ExprVar x1)
                   4 -> do x1 <- get
                           return (ExprString x1)
                   5 -> do x1 <- get
                           x2 <- get
                           return (ExprIndex x1 x2)
                   6 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (ExprSlice x1 x2 x3)
                   7 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (ExprSliceOff x1 x2 x3)
                   8 -> do x1 <- get
                           return (ExprConcat x1)
                   9 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (ExprCond x1 x2 x3)
                   10 -> do x1 <- get
                            x2 <- get
                            return (ExprUnary x1 x2)
                   11 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (ExprBinary x1 x2 x3)
                   12 -> do x1 <- get
                            x2 <- get
                            return (ExprFunCall x1 x2)
                   _ -> error "Corrupted binary data for Expr"


instance Binary Stmt where
        put x
          = case x of
                Assign x1 x2 -> do putWord8 0
                                   put x1
                                   put x2
                If x1 x2 x3 -> do putWord8 1
                                  put x1
                                  put x2
                                  put x3
                Case x1 x2 x3 -> do putWord8 2
                                    put x1
                                    put x2
                                    put x3
                Seq x1 -> do putWord8 3
                             put x1
                FunCallStmt x1 x2 -> do putWord8 4
                                        put x1
                                        put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           return (Assign x1 x2)
                   1 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (If x1 x2 x3)
                   2 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (Case x1 x2 x3)
                   3 -> do x1 <- get
                           return (Seq x1)
                   4 -> do x1 <- get
                           x2 <- get
                           return (FunCallStmt x1 x2)
                   _ -> error "Corrupted binary data for Stmt"


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
