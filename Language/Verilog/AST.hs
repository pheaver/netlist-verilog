-- -----------------------------------------------------------------------------
-- Copyright (c) 2010 Signali Corp.
--
-- An abstract syntax tree (AST) for Verilog, taken straight form
-- http://www.verilog.com/VerilogBNF.html.  This AST is very close to the
-- concrete syntax, and is not meant to operated on, other than generating,
-- pretty printing, and parsing.  To do anything more advanced, it should be
-- converted into another form.
-- -----------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, TypeOperators #-}
{-# OPTIONS_DERIVE --append -d Binary #-}

module Language.Verilog.AST ( module Language.Verilog.AST
                            , module Operators
                            ) where

import Data.Binary      ( Binary(..), putWord8, getWord8 )
import Data.Generics    ( Data, Typeable )

import Operators

-- -----------------------------------------------------------------------------

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- 1. Source Text

newtype Verilog = Verilog [Description]

type Description = Module

{-
data Description
  = ModuleDescription Module
  -- TODO: | UDP -- User-defined primitive
  deriving (Eq, Ord, Show, Data, Typeable)
-}

data Module
  = Module Ident   -- name of module
           [Ident] -- list of ports
                   -- this is a more complicated "Port" in the spec
           [Item]  -- module body
  deriving (Eq, Ord, Show, Data, Typeable)

data Item
  = ParamDeclItem ParamDecl
  | InputDeclItem InputDecl
  | OutputDeclItem OutputDecl
  | InOutDeclItem InOutDecl
  | NetDeclItem NetDecl
  | RegDeclItem RegDecl
  | TimeDeclItem TimeDecl
  | IntegerDeclItem IntegerDecl
  | RealDeclItem RealDecl
  | EventDeclItem EventDecl
  -- TODO: GateDecl
  -- TODO: UDPInst
  | ModuleInstItem ModuleInst 
  -- TODO: ParamOverride
  | AssignItem (Maybe DriveStrength) (Maybe Delay) [Assignment]
  -- TODO: SpecifyBlock
  | InitialItem Statement
  | AlwaysItem Statement
  -- TODO: Task
  -- TODO: Function
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- 2. Declarations

newtype ParamDecl
  = ParamDecl [ParamAssign]
  deriving (Eq, Ord, Show, Data, Typeable)

data InputDecl
  = InputDecl (Maybe Range) [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

data OutputDecl
  = OutputDecl (Maybe Range) [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

data InOutDecl
  = InOutDecl (Maybe Range) [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

data NetDecl
  = NetDecl NetType (Maybe ExpandRange) (Maybe Delay) [Ident]
  | NetDeclAssign NetType (Maybe DriveStrength) (Maybe ExpandRange) (Maybe Delay) [Assignment]
  -- TODO: net decl: trireg <charge_strength>? <expandrange>? <delay>?
  deriving (Eq, Ord, Show, Data, Typeable)

data RegDecl
  = RegDecl (Maybe Range) [RegVar]
  deriving (Eq, Ord, Show, Data, Typeable)

newtype TimeDecl
  = TimeDecl [RegVar]
  deriving (Eq, Ord, Show, Data, Typeable)

newtype IntegerDecl
  = IntegerDecl [RegVar]
  deriving (Eq, Ord, Show, Data, Typeable)

newtype RealDecl
  = RealDecl [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

newtype EventDecl
  = EventDecl [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- 3. Primitive Instances

data GateDecl
  = GateDecl GateType (Maybe DriveStrength) (Maybe Delay) [GateInst]
  deriving (Eq, Ord, Show, Data, Typeable)

data GateInst
  = GateInst (Maybe GateInstName) [Terminal]
  deriving (Eq, Ord, Show, Data, Typeable)

data GateInstName
  = GateInstName Ident (Maybe Range)
  deriving (Eq, Ord, Show, Data, Typeable)

type GateType = String

gateTypes :: [GateType]
gateTypes
  = [ "and", "nand", "or", "nor", "xor", "xnor", "buf", "bufif0", "bufif1"
    , "not", "notif0", "notif1", "pulldown", "pullup", "nmos", "rnmos", "pmos"
    , "rpmos", "cmos", "rcmos", "tran", "rtran", "tranif0", "rtranif0"
    , "tranif1", "rtranif1"
    ]

validGateType :: GateType -> Bool
validGateType = flip elem gateTypes

data Terminal
  = ExprTerminal Expression
  | IdentTerminal Ident
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- 4. Module Instantiations

data ModuleInst
  = ModuleInst Ident         -- <name_of_module>
               [Parameter]   -- <parameter_value_assignment>
               [Instance]    -- <module_instance>
  deriving (Eq, Ord, Show, Data, Typeable)

-- the spec says this is just one or more expressions, but that doesn't seem
-- right to me.  It should be a mapping of parameter names to expression values.
-- I suspect that the spec is outdated (applies to an older Verilog standard).
data Parameter
  = Parameter Ident Expression
  deriving (Eq, Ord, Show, Data, Typeable)

data Instance
  = Instance Ident          -- <name_of_instance>
             (Maybe Range)  -- <range>?  -- not sure what this is for...
             Connections    -- <list_of_module_connections>
  deriving (Eq, Ord, Show, Data, Typeable)

data Connections
  = Connections [Expression]
  | NamedConnections [NamedConnection]
  deriving (Eq, Ord, Show, Data, Typeable)

-- we use this for both parameter_value_assignment and named_port_connection
data NamedConnection
  = NamedConnection Ident Expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- ----------------------------------------------------------------------------
-- 5. Behavioral Statements

data Statement
  = BlockingAssignment LValue (Maybe DelayOrEventControl) Expression
  | NonBlockingAssignment LValue (Maybe DelayOrEventControl) Expression
  | IfStmt Expression (Maybe Statement) (Maybe Statement)
  -- TODO: CaseStmt CaseType Expression [CaseItem]
  | ForeverStmt Statement
  | RepeatStmt Expression Statement
  | WhileStmt Expression Statement
  | ForStmt Assignment Expression Assignment Statement
  | DelayOrEventControlStmt DelayOrEventControl (Maybe Statement)
  | WaitStmt Expression (Maybe Statement)
  -- TODO: -> <name_of_event>
  | SeqBlock (Maybe Ident) [BlockDecl] [Statement]
  | ParBlock (Maybe Ident) [BlockDecl] [Statement]
  -- TODO: <task_enable>
  -- TODO: <system_task_enable>
  -- TODO: DisableStmt Ident
  | AssignStmt Assignment
  | DeAssignStmt LValue
  | ForceStmt Assignment
  | ReleaseStmt LValue
  | Foo String
  deriving (Eq, Ord, Show, Data, Typeable)

data Assignment
  = Assignment LValue Expression
  deriving (Eq, Ord, Show, Data, Typeable)

data BlockDecl
  = ParamDeclBlock ParamDecl
  | RegDeclBlock RegDecl
  | IntegerDeclBlock IntegerDecl
  | RealDeclBlock RealDecl
  | TimeDeclBlock TimeDecl
  | EventDeclBlock EventDecl
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- 7. Expressions

data Expression
  -- TODO: support X, x, z, and Z in numbers
  -- TODO: support fractional numbers (e.g. 3.14)
  -- TODO: support exponent numbers (e.g. 3e10)
  = ExprNum Integer
  | ExprSizedNum Integer Integer
  | ExprVar Ident
  | ExprString String
  | ExprIndex Ident Expression
  | ExprSlice Ident ConstExpr ConstExpr
  -- these next two aren't in the spec, but they're certainly in the Verilog standard
  | ExprSlicePlus Ident Expression ConstExpr
  | ExprSliceMinus Ident Expression ConstExpr
  | ExprConcat [Expression]
  | ExprMultiConcat Expression [Expression] -- a.k.a. "replication operator"
  -- TODO: ExprFunCall
  -- TODO: <mintypmax_expression>
  | ExprUnary UnaryOp Expression
  | ExprBinary BinaryOp Expression Expression
  | ExprCond Expression Expression Expression
  deriving (Eq, Ord, Show, Data, Typeable)

type ConstExpr = Expression

-- data Base = BinBase | OctBase | DecBase | HexBase
--   deriving (Eq, Ord, Show, Data, Typeable)

-- A LValue is supposed to be limited to the following:
--    ident | ident[expr] | ident[const_expr : const_expr ] | concatentation
-- However, we don't make that restriction in our AST.
type LValue = Expression

-- -----------------------------------------------------------------------------
-- Miscellaneous

data ParamAssign
  = ParamAssign Ident ConstExpr
  deriving (Eq, Ord, Show, Data, Typeable)

data ExpandRange
  = SimpleRange Range
  | ScalaredRange Range
  | VectoredRange Range
  deriving (Eq, Ord, Show, Data, Typeable)

data Range
  = Range ConstExpr ConstExpr
  deriving (Eq, Ord, Show, Data, Typeable)

-- TODO: shouldn't we be able to give initial values to regs?
data RegVar
  = RegVar Ident (Maybe Range)
  deriving (Eq, Ord, Show, Data, Typeable)

data DelayOrEventControl
  = DelayControl DelayControl
  | EventControl EventControl
  | RepeatControl Expression EventControl
  deriving (Eq, Ord, Show, Data, Typeable)

type DelayControl = Delay
type EventControl = EventExpr

-- the actual syntax for <delay> is:
-- <number> | <identifier> | <mintypmax_expression>,
-- where mintypmax_expression can be expression.
-- so, we just use expression here.

type Delay = Expression

data EventExpr
  = EventExpr Expression
  | EventPosedge ScalarEventExpr
  | EventNegedge ScalarEventExpr
  | EventOr EventExpr EventExpr
  deriving (Eq, Ord, Show, Data, Typeable)

-- From the spec:
-- "Scalar event expression is an expression that resolves to a one bit value."
type ScalarEventExpr = Expression

type ChargeStrength = String

charge_strengths :: [ChargeStrength]
charge_strengths = [ "small", "medium", "large" ]

data DriveStrength
  = Strength01 Strength0 Strength1
  | Strength10 Strength1 Strength0
  deriving (Eq, Ord, Show, Data, Typeable)

type Strength0 = String
type Strength1 = String

strength0s :: [Strength0]
strength0s
  = [ "supply0", "strong0", "pull0", "weak0", "highz0" ]

strength1s :: [Strength1]
strength1s
  = [ "supply1", "strong1", "pull1", "weak1", "highz0" ]

type NetType = String

net_types :: [NetType]
net_types
  = [ "wire", "tri", "tri1", "supply0", "wand"
    , "triand", "tri0", "supply1", "wor", "trior", "trireg"
    ]

-- -----------------------------------------------------------------------------
-- GENERATED START

 
instance Binary Ident where
        put (Ident x1) = put x1
        get
          = do x1 <- get
               return (Ident x1)

 
instance Binary Verilog where
        put (Verilog x1) = put x1
        get
          = do x1 <- get
               return (Verilog x1)

 
instance Binary Module where
        put (Module x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (Module x1 x2 x3)

 
instance Binary Item where
        put x
          = case x of
                ParamDeclItem x1 -> do putWord8 0
                                       put x1
                InputDeclItem x1 -> do putWord8 1
                                       put x1
                OutputDeclItem x1 -> do putWord8 2
                                        put x1
                InOutDeclItem x1 -> do putWord8 3
                                       put x1
                NetDeclItem x1 -> do putWord8 4
                                     put x1
                RegDeclItem x1 -> do putWord8 5
                                     put x1
                TimeDeclItem x1 -> do putWord8 6
                                      put x1
                IntegerDeclItem x1 -> do putWord8 7
                                         put x1
                RealDeclItem x1 -> do putWord8 8
                                      put x1
                EventDeclItem x1 -> do putWord8 9
                                       put x1
                ModuleInstItem x1 -> do putWord8 10
                                        put x1
                AssignItem x1 x2 x3 -> do putWord8 11
                                          put x1
                                          put x2
                                          put x3
                InitialItem x1 -> do putWord8 12
                                     put x1
                AlwaysItem x1 -> do putWord8 13
                                    put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (ParamDeclItem x1)
                   1 -> do x1 <- get
                           return (InputDeclItem x1)
                   2 -> do x1 <- get
                           return (OutputDeclItem x1)
                   3 -> do x1 <- get
                           return (InOutDeclItem x1)
                   4 -> do x1 <- get
                           return (NetDeclItem x1)
                   5 -> do x1 <- get
                           return (RegDeclItem x1)
                   6 -> do x1 <- get
                           return (TimeDeclItem x1)
                   7 -> do x1 <- get
                           return (IntegerDeclItem x1)
                   8 -> do x1 <- get
                           return (RealDeclItem x1)
                   9 -> do x1 <- get
                           return (EventDeclItem x1)
                   10 -> do x1 <- get
                            return (ModuleInstItem x1)
                   11 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (AssignItem x1 x2 x3)
                   12 -> do x1 <- get
                            return (InitialItem x1)
                   13 -> do x1 <- get
                            return (AlwaysItem x1)
                   _ -> error "Corrupted binary data for Item"

 
instance Binary ParamDecl where
        put (ParamDecl x1) = put x1
        get
          = do x1 <- get
               return (ParamDecl x1)

 
instance Binary InputDecl where
        put (InputDecl x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (InputDecl x1 x2)

 
instance Binary OutputDecl where
        put (OutputDecl x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (OutputDecl x1 x2)

 
instance Binary InOutDecl where
        put (InOutDecl x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (InOutDecl x1 x2)

 
instance Binary NetDecl where
        put x
          = case x of
                NetDecl x1 x2 x3 x4 -> do putWord8 0
                                          put x1
                                          put x2
                                          put x3
                                          put x4
                NetDeclAssign x1 x2 x3 x4 x5 -> do putWord8 1
                                                   put x1
                                                   put x2
                                                   put x3
                                                   put x4
                                                   put x5
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (NetDecl x1 x2 x3 x4)
                   1 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           x5 <- get
                           return (NetDeclAssign x1 x2 x3 x4 x5)
                   _ -> error "Corrupted binary data for NetDecl"

 
instance Binary RegDecl where
        put (RegDecl x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (RegDecl x1 x2)

 
instance Binary TimeDecl where
        put (TimeDecl x1) = put x1
        get
          = do x1 <- get
               return (TimeDecl x1)

 
instance Binary IntegerDecl where
        put (IntegerDecl x1) = put x1
        get
          = do x1 <- get
               return (IntegerDecl x1)

 
instance Binary RealDecl where
        put (RealDecl x1) = put x1
        get
          = do x1 <- get
               return (RealDecl x1)

 
instance Binary EventDecl where
        put (EventDecl x1) = put x1
        get
          = do x1 <- get
               return (EventDecl x1)

 
instance Binary GateDecl where
        put (GateDecl x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (GateDecl x1 x2 x3 x4)

 
instance Binary GateInst where
        put (GateInst x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (GateInst x1 x2)

 
instance Binary GateInstName where
        put (GateInstName x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (GateInstName x1 x2)

 
instance Binary Terminal where
        put x
          = case x of
                ExprTerminal x1 -> do putWord8 0
                                      put x1
                IdentTerminal x1 -> do putWord8 1
                                       put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (ExprTerminal x1)
                   1 -> do x1 <- get
                           return (IdentTerminal x1)
                   _ -> error "Corrupted binary data for Terminal"

 
instance Binary ModuleInst where
        put (ModuleInst x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (ModuleInst x1 x2 x3)

 
instance Binary Parameter where
        put (Parameter x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Parameter x1 x2)

 
instance Binary Instance where
        put (Instance x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (Instance x1 x2 x3)

 
instance Binary Connections where
        put x
          = case x of
                Connections x1 -> do putWord8 0
                                     put x1
                NamedConnections x1 -> do putWord8 1
                                          put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (Connections x1)
                   1 -> do x1 <- get
                           return (NamedConnections x1)
                   _ -> error "Corrupted binary data for Connections"

 
instance Binary NamedConnection where
        put (NamedConnection x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (NamedConnection x1 x2)

 
instance Binary Statement where
        put x
          = case x of
                BlockingAssignment x1 x2 x3 -> do putWord8 0
                                                  put x1
                                                  put x2
                                                  put x3
                NonBlockingAssignment x1 x2 x3 -> do putWord8 1
                                                     put x1
                                                     put x2
                                                     put x3
                IfStmt x1 x2 x3 -> do putWord8 2
                                      put x1
                                      put x2
                                      put x3
                ForeverStmt x1 -> do putWord8 3
                                     put x1
                RepeatStmt x1 x2 -> do putWord8 4
                                       put x1
                                       put x2
                WhileStmt x1 x2 -> do putWord8 5
                                      put x1
                                      put x2
                ForStmt x1 x2 x3 x4 -> do putWord8 6
                                          put x1
                                          put x2
                                          put x3
                                          put x4
                DelayOrEventControlStmt x1 x2 -> do putWord8 7
                                                    put x1
                                                    put x2
                WaitStmt x1 x2 -> do putWord8 8
                                     put x1
                                     put x2
                SeqBlock x1 x2 x3 -> do putWord8 9
                                        put x1
                                        put x2
                                        put x3
                ParBlock x1 x2 x3 -> do putWord8 10
                                        put x1
                                        put x2
                                        put x3
                AssignStmt x1 -> do putWord8 11
                                    put x1
                DeAssignStmt x1 -> do putWord8 12
                                      put x1
                ForceStmt x1 -> do putWord8 13
                                   put x1
                ReleaseStmt x1 -> do putWord8 14
                                     put x1
                Foo x1 -> do putWord8 15
                             put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (BlockingAssignment x1 x2 x3)
                   1 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (NonBlockingAssignment x1 x2 x3)
                   2 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (IfStmt x1 x2 x3)
                   3 -> do x1 <- get
                           return (ForeverStmt x1)
                   4 -> do x1 <- get
                           x2 <- get
                           return (RepeatStmt x1 x2)
                   5 -> do x1 <- get
                           x2 <- get
                           return (WhileStmt x1 x2)
                   6 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (ForStmt x1 x2 x3 x4)
                   7 -> do x1 <- get
                           x2 <- get
                           return (DelayOrEventControlStmt x1 x2)
                   8 -> do x1 <- get
                           x2 <- get
                           return (WaitStmt x1 x2)
                   9 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           return (SeqBlock x1 x2 x3)
                   10 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (ParBlock x1 x2 x3)
                   11 -> do x1 <- get
                            return (AssignStmt x1)
                   12 -> do x1 <- get
                            return (DeAssignStmt x1)
                   13 -> do x1 <- get
                            return (ForceStmt x1)
                   14 -> do x1 <- get
                            return (ReleaseStmt x1)
                   15 -> do x1 <- get
                            return (Foo x1)
                   _ -> error "Corrupted binary data for Statement"

 
instance Binary Assignment where
        put (Assignment x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Assignment x1 x2)

 
instance Binary BlockDecl where
        put x
          = case x of
                ParamDeclBlock x1 -> do putWord8 0
                                        put x1
                RegDeclBlock x1 -> do putWord8 1
                                      put x1
                IntegerDeclBlock x1 -> do putWord8 2
                                          put x1
                RealDeclBlock x1 -> do putWord8 3
                                       put x1
                TimeDeclBlock x1 -> do putWord8 4
                                       put x1
                EventDeclBlock x1 -> do putWord8 5
                                        put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (ParamDeclBlock x1)
                   1 -> do x1 <- get
                           return (RegDeclBlock x1)
                   2 -> do x1 <- get
                           return (IntegerDeclBlock x1)
                   3 -> do x1 <- get
                           return (RealDeclBlock x1)
                   4 -> do x1 <- get
                           return (TimeDeclBlock x1)
                   5 -> do x1 <- get
                           return (EventDeclBlock x1)
                   _ -> error "Corrupted binary data for BlockDecl"

 
instance Binary Expression where
        put x
          = case x of
                ExprNum x1 -> do putWord8 0
                                 put x1
                ExprSizedNum x1 x2 -> do putWord8 1
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
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (ExprNum x1)
                   1 -> do x1 <- get
                           x2 <- get
                           return (ExprSizedNum x1 x2)
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
                   _ -> error "Corrupted binary data for Expression"

 
instance Binary ParamAssign where
        put (ParamAssign x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (ParamAssign x1 x2)

 
instance Binary ExpandRange where
        put x
          = case x of
                SimpleRange x1 -> do putWord8 0
                                     put x1
                ScalaredRange x1 -> do putWord8 1
                                       put x1
                VectoredRange x1 -> do putWord8 2
                                       put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (SimpleRange x1)
                   1 -> do x1 <- get
                           return (ScalaredRange x1)
                   2 -> do x1 <- get
                           return (VectoredRange x1)
                   _ -> error "Corrupted binary data for ExpandRange"

 
instance Binary Range where
        put (Range x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Range x1 x2)

 
instance Binary RegVar where
        put (RegVar x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (RegVar x1 x2)

 
instance Binary DelayOrEventControl where
        put x
          = case x of
                DelayControl x1 -> do putWord8 0
                                      put x1
                EventControl x1 -> do putWord8 1
                                      put x1
                RepeatControl x1 x2 -> do putWord8 2
                                          put x1
                                          put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (DelayControl x1)
                   1 -> do x1 <- get
                           return (EventControl x1)
                   2 -> do x1 <- get
                           x2 <- get
                           return (RepeatControl x1 x2)
                   _ -> error "Corrupted binary data for DelayOrEventControl"

 
instance Binary EventExpr where
        put x
          = case x of
                EventExpr x1 -> do putWord8 0
                                   put x1
                EventPosedge x1 -> do putWord8 1
                                      put x1
                EventNegedge x1 -> do putWord8 2
                                      put x1
                EventOr x1 x2 -> do putWord8 3
                                    put x1
                                    put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (EventExpr x1)
                   1 -> do x1 <- get
                           return (EventPosedge x1)
                   2 -> do x1 <- get
                           return (EventNegedge x1)
                   3 -> do x1 <- get
                           x2 <- get
                           return (EventOr x1 x2)
                   _ -> error "Corrupted binary data for EventExpr"

 
instance Binary DriveStrength where
        put x
          = case x of
                Strength01 x1 x2 -> do putWord8 0
                                       put x1
                                       put x2
                Strength10 x1 x2 -> do putWord8 1
                                       put x1
                                       put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           return (Strength01 x1 x2)
                   1 -> do x1 <- get
                           x2 <- get
                           return (Strength10 x1 x2)
                   _ -> error "Corrupted binary data for DriveStrength"
-- GENERATED STOP
