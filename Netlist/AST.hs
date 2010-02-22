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

module Netlist.AST where

import Data.Binary      ( Binary(..), putWord8, getWord8 )
import Data.Generics	( Data, Typeable )

import Operators

-- -----------------------------------------------------------------------------

-- corresponds to a Verilog "module" or VHDL "entity"
data Module
  = Module { module_name    :: Ident
           , module_inputs  :: [(Ident, Size)]
           , module_outputs :: [(Ident, Size)]
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
  = NetDecl Ident Size (Maybe Expr)
  | NetAssign Ident Expr

  -- A mem (aka 'reg') is stateful.  it can be assigned by a non-blocking
  -- assignment (or blocking, but we don't support those yet) within a process.
  -- TODO: support 2-dimensional memories
  -- TODO: support optional initial value
  | MemDecl Ident Size

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

data Event
  = Event Ident Edge
  deriving (Eq, Ord, Show, Data, Typeable)

data Edge
  = PosEdge
  | NegEdge
  -- TODO: AnyEdge?
  deriving (Eq, Ord, Show, Data, Typeable)

-- Expr is combination of VHDL and Verilog expressions.  We use the operators
-- from the Operators module, which includes almost all VHDL and Verilog operators.

-- In VHDL, concatenation is a binary operator, but in Verilog it takes any
-- number of arguments.  In this AST, we define it like the Verilog operator.
-- If we translate to VHDL, we have to convert it to the VHDL binary operator.

-- There are some HDL operators that we don't represent here.  For example, in
-- Verilog there is a multiple concatenation (a.k.a. replication) operator,
-- which we don't bother to support.

data Expr
  -- terminal nodes
  = ExprNum (Maybe Size) Integer
  | ExprVar Ident
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
  = Assign LValue Expr          -- non-blocking assignment
  | If Expr Stmt (Maybe Stmt)  -- if statement
  | Seq [Stmt]                 -- multiple statements (between 'begin' and 'end')
  deriving (Eq, Ord, Show, Data, Typeable)

-- not all expressions are allowed on the LHS of an assignment, but we're lazy
-- and don't enforce that restriction in the AST.
type LValue = Expr

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
                MemDecl x1 x2 -> do putWord8 2
                                    put x1
                                    put x2
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
                           return (MemDecl x1 x2)
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
                ExprNum x1 x2 -> do putWord8 0
                                    put x1
                                    put x2
                ExprVar x1 -> do putWord8 1
                                 put x1
                ExprIndex x1 x2 -> do putWord8 2
                                      put x1
                                      put x2
                ExprSlice x1 x2 x3 -> do putWord8 3
                                         put x1
                                         put x2
                                         put x3
                ExprSliceOff x1 x2 x3 -> do putWord8 4
                                            put x1
                                            put x2
                                            put x3
                ExprConcat x1 -> do putWord8 5
                                    put x1
                ExprCond x1 x2 x3 -> do putWord8 6
                                        put x1
                                        put x2
                                        put x3
                ExprUnary x1 x2 -> do putWord8 7
                                      put x1
                                      put x2
                ExprBinary x1 x2 x3 -> do putWord8 8
                                          put x1
                                          put x2
                                          put x3
                ExprFunCall x1 x2 -> do putWord8 9
                                        put x1
                                        put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           return (ExprNum x1 x2)
                   1 -> do x1 <- get
                           return (ExprVar x1)
                   2 -> do x1 <- get
                           x2 <- get
                           return (ExprIndex x1 x2)
                   3 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (ExprSlice x1 x2 x3)
                   4 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (ExprSliceOff x1 x2 x3)
                   5 -> do x1 <- get
                           return (ExprConcat x1)
                   6 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (ExprCond x1 x2 x3)
                   7 -> do x1 <- get
                           x2 <- get
                           return (ExprUnary x1 x2)
                   8 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (ExprBinary x1 x2 x3)
                   9 -> do x1 <- get
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
                Seq x1 -> do putWord8 2
                             put x1
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
                           return (Seq x1)
                   _ -> error "Corrupted binary data for Stmt"
-- GENERATED STOP
