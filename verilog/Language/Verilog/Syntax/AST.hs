--------------------------------------------------------------------------------
-- |
-- Module       :  Language.Verilog.Syntax.AST
-- Copyright    :  (c) Signali Corp. 2010
-- License      :  All rights reserved
--
-- Maintainer   : pweaver@signalicorp.com
-- Stability    : experimental
-- Portability  : ghc
--
-- An abstract syntax tree (AST) for Verilog.  We used the following to help
-- write the definition of the AST:
--
--  * <http://www.verilog.com/VerilogBNF.html>
--
--  * <http://www.hdlworks.com/hdl_corner/verilog_ref/index.html>
--
--  * <http://en.wikipedia.org/wiki/Verilog>
--
-- This AST is very close to the concrete syntax, and is only meant for
-- generating, pretty printing, and parsing.  To do anything more advanced, it
-- should be converted into another form.
--
-- The AST is incomplete in many places, and deviates from the spec in a few
-- places, too.  We've made an effort to document these places.
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, TypeOperators #-}
{-# OPTIONS_DERIVE --append -d Binary #-}

module Language.Verilog.Syntax.AST
  (
  -- * The top-level types
  Verilog(..), Module(..), Description(..),

  -- * User-defined primitives
  UDP(..), UDPDecl(..), UDPInitialStatement(..),
  TableDefinition(..), CombinationalEntry(..), SequentialEntry(..),
  LevelSymbol, levelSymbols, validLevelSymbol,
  OutputSymbol, outputSymbols, validOutputSymbol,
  NextState, nextStates, validNextState,
  Edge(..), EdgeSymbol, edgeSymbols, validEdgeSymbol,

  -- * Items and Declarations
  Item(..), ParamDecl(..), InputDecl(..), OutputDecl(..), InOutDecl(..),
  NetDecl(..), RegDecl(..), RegType(..), EventDecl(..),

  -- * Primitive Instances
  PrimitiveInst(..), PrimInst(..), PrimInstName(..), PrimType(..),

  -- * Module Instantiations
  ModuleInst(..), Parameter(..), Instance(..), Connections(..), NamedConnection(..),

  -- * Behavioral Statements
  Statement(..), Assignment(..), LValue,
  CaseWord(..), CaseItem(..), BlockDecl(..),

  -- * Expressions
  Expression(..), ConstExpr, Number(..), Base(..), Sign(..), intExpr,
  UnaryOp(..), BinaryOp(..),

  -- * Miscellaneous
  Ident(..), ParamAssign(..), ExpandRange(..), Range(..), RegVar(..),
  AssignmentControl(..), DelayControl, EventControl(..), Delay,
  EventExpr(..), ScalarEventExpr,
  ChargeStrength, DriveStrength(..),
  Strength0(..), Strength1(..), NetType(..)
  ) where

import Data.Binary      ( Binary(..), putWord8, getWord8 )
import Data.Generics    ( Data, Typeable )

import Language.Verilog.Syntax.Ident
import Language.Verilog.Syntax.Expression

-- -----------------------------------------------------------------------------
-- 1. Source Text

newtype Verilog = Verilog [Description]
  deriving (Eq, Ord, Show, Data, Typeable)

-- | This should be module or user-defined primitive, but for now we are only
-- supporting module.
data Description
  = ModuleDescription Module
  | UDPDescription UDP
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A top-level module has the module name, the list of ports (both input and
-- output), and the body of the module (a list of declarations).  In the spec,
-- the ports have a more complicated type than simply @Ident@.
data Module
  = Module Ident     -- The name of module
           [Ident]   -- The list of ports, including both inputs and outputs
                     -- In the spec, this is a more complicated type.
           [Item]    -- The module's body, a list of declarations.
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A declaration.
data Item
  = ParamDeclItem ParamDecl
  | InputDeclItem InputDecl
  | OutputDeclItem OutputDecl
  | InOutDeclItem InOutDecl
  | NetDeclItem NetDecl
  | RegDeclItem RegDecl
  | EventDeclItem EventDecl
  | PrimitiveInstItem PrimitiveInst
  -- TODO: UDPInst
  | ModuleInstItem ModuleInst
  | ParamOverrideItem [ParamAssign]
  | AssignItem (Maybe DriveStrength) (Maybe Delay) [Assignment]
  -- TODO: SpecifyBlock
  | InitialItem Statement
  | AlwaysItem Statement
  -- TODO: Task
  -- TODO: Function
  deriving (Eq, Ord, Show, Data, Typeable)

-- --------------------

-- | User-defined primitive (UDP)
data UDP
  = UDP Ident     -- Name of UDP
        Ident     -- Name of output variable
        [Ident]   -- Name of input variables
        [UDPDecl] -- input/output/reg declarations
        (Maybe UDPInitialStatement)
        TableDefinition
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A UDP can have output, input, and reg declarations.
data UDPDecl
  = UDPOutputDecl OutputDecl
  | UDPInputDecl InputDecl
  | UDPRegDecl Ident
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A UDP initial statement defines the initial value of a UDP.
data UDPInitialStatement
  = UDPInitialStatement Ident Expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- According to the spec, the initial value of a UDP can be any of the
-- following, but we don't make that restriction in the AST.
{-
<init_val>
::= 1'b0
||= 1'b1
||= 1'bx
||= 1'bX
||= 1'B0
||= 1'B1
||= 1'Bx
||= 1'BX
||= 1
||= 0
-}

-- | A UDP's definition is a truth table.
data TableDefinition
  = CombinationalTable [CombinationalEntry]
  | SequentialTable [SequentialEntry]
  deriving (Eq, Ord, Show, Data, Typeable)

-- | An entry in a combinational table.
data CombinationalEntry
  = CombinationalEntry [LevelSymbol] OutputSymbol
  deriving (Eq, Ord, Show, Data, Typeable)

type LevelSymbol = Char

-- | A level symbol is one of the following characters:
-- 0   1   x   X   ?   b   B
levelSymbols :: [LevelSymbol]
levelSymbols = "01xX?bB"

validLevelSymbol :: Char -> Bool
validLevelSymbol = flip elem levelSymbols

type OutputSymbol = Char

-- | An output symbol is one of the following characters:
-- 0   1   x   X   ?   b   B
outputSymbols :: [OutputSymbol]
outputSymbols = "01xX"

validOutputSymbol :: Char -> Bool
validOutputSymbol = flip elem outputSymbols

data SequentialEntry
  = SequentialEntry [Either LevelSymbol Edge] LevelSymbol NextState
  deriving (Eq, Ord, Show, Data, Typeable)

data Edge
  = EdgeLevels LevelSymbol LevelSymbol
  | EdgeSymbol EdgeSymbol
  deriving (Eq, Ord, Show, Data, Typeable)

type EdgeSymbol = Char

edgeSymbols :: [Char]
edgeSymbols = "rRfFpPnN*"

validEdgeSymbol :: Char -> Bool
validEdgeSymbol = flip elem edgeSymbols

type NextState = Char

nextStates :: [NextState]
nextStates = outputSymbols ++ "-"

validNextState :: NextState -> Bool
validNextState = flip elem nextStates

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
  | NetDeclAssign NetType (Maybe DriveStrength) (Maybe ExpandRange)
    (Maybe Delay) [(Ident, Expression)]
  -- TODO trireg <charge_strength>? <expandrange>? <delay>?
  -- TODO allow mixing assignment and non-assignments, such as:
  --      wire x = 1, y, z = 0;
  deriving (Eq, Ord, Show, Data, Typeable)

-- note that only the "reg" type allows for a vector range before the RegVar,
-- but we don't make that restriction in the AST.
data RegDecl
  = RegDecl RegType (Maybe Range) [RegVar]
  deriving (Eq, Ord, Show, Data, Typeable)

-- TODO support multi-dimensional array
data RegVar
  = RegVar Ident (Maybe Expression) -- ^ with optional initial value
  | MemVar Ident Range              -- ^ array
  deriving (Eq, Ord, Show, Data, Typeable)

newtype EventDecl
  = EventDecl [Ident]
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- 3. Primitive Instances

data PrimitiveInst
  = PrimitiveInst PrimType (Maybe DriveStrength) (Maybe Delay) [PrimInst]
  deriving (Eq, Ord, Show, Data, Typeable)

data PrimInst
  = PrimInst (Maybe PrimInstName) [Expression]
  deriving (Eq, Ord, Show, Data, Typeable)

data PrimInstName
  = PrimInstName Ident (Maybe Range)
  deriving (Eq, Ord, Show, Data, Typeable)

data PrimType
  = Gate_and | Gate_nand | Gate_or | Gate_nor | Gate_xor | Gate_xnor | Gate_not
  | Gate_buf | Gate_bufif0 | Gate_bufif1
  | Gate_notif0 | Gate_notify1 | Gate_pulldown | Gate_pullup
  | Gate_nmos | Gate_rnmos | Gate_pmos | Gate_rpmos | Gate_cmos | Gate_rcmos
  | Gate_tran | Gate_rtran
  | Gate_tranif0 | Gate_rtranif0 | Gate_tranif1 | Gate_rtranif1
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show PrimType where
  show Gate_and      = "and"
  show Gate_nand     = "nand"
  show Gate_or       = "or"
  show Gate_nor      = "nor"
  show Gate_xor      = "xor"
  show Gate_xnor     = "xnor"
  show Gate_not      = "not"
  show Gate_buf      = "buf"
  show Gate_bufif0   = "bufif0"
  show Gate_bufif1   = "bufif1"
  show Gate_notif0   = "notif0"
  show Gate_notify1  = "notify1"
  show Gate_pulldown = "pulldown"
  show Gate_pullup   = "pullup"
  show Gate_nmos     = "nmos"
  show Gate_rnmos    = "rnmos"
  show Gate_pmos     = "pmos"
  show Gate_rpmos    = "rpmos"
  show Gate_cmos     = "cmos"
  show Gate_rcmos    = "rcmos"
  show Gate_tran     = "tran"
  show Gate_rtran    = "rtran"
  show Gate_tranif0  = "tranif0"
  show Gate_rtranif0 = "rtranif0"
  show Gate_tranif1  = "tranif1"
  show Gate_rtranif1 = "rtranif1"

-- -----------------------------------------------------------------------------
-- 4. Module Instantiations

-- | A module instantiation.  In Verilog, a module instantiation can list
-- multiple instances of the same module.  So, the arguments to @ModuleInst@ are
-- the name of the module (not the name of the instantiation), the list of
-- parameter assignments, and the list of module instances.  Each @Instance@ has
-- a name and port connections.
data ModuleInst
  = ModuleInst Ident         -- Name of the module
               [Parameter]   -- Parameter value assignments
               [Instance]    -- Module instances
  deriving (Eq, Ord, Show, Data, Typeable)

-- the spec says this is just one or more expressions, but that doesn't seem
-- right to me.  It should be a mapping of parameter names to expression values.
-- I suspect that the spec is outdated (applies to an older Verilog standard).
-- | A parameter value assignment is used in a module instance to associate a
-- parameter with a value.
data Parameter
  = Parameter Ident Expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A module instance.  The name of the module and the parameter assignments
-- are defined in @ModuleInst@, which has any number of @Instance@s.  The
-- instance itself has a name and a list of port connections.
data Instance
  = Instance Ident          -- Name of the instance
             (Maybe Range)  -- I'm actually not sure what this is for!
             Connections    -- The (input and output) port connections.
  deriving (Eq, Ord, Show, Data, Typeable)

-- | Connections in a module instance can be all nnammed or all unnamed.  When
-- they are unnamed, this means they are positional, like in most programming
-- languages.  However, when the formal ports are named, then you can specify
-- the connections in any order.
data Connections
  = Connections [Expression]
  | NamedConnections [NamedConnection]
  deriving (Eq, Ord, Show, Data, Typeable)

-- | A named connection, like a parameter value assignment, associates a port
-- with a value in a module instance.
data NamedConnection
  = NamedConnection Ident Expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- ----------------------------------------------------------------------------
-- 5. Behavioral Statements

-- | Behavioral statements.
data Statement
  -- | blocking assignment, e.g. @x \= y@
  = BlockingAssignment LValue (Maybe AssignmentControl) Expression
  -- | non-blocking assignment, e.g. @x \<= y@
  | NonBlockingAssignment LValue (Maybe AssignmentControl) Expression
  -- | @if@ statement.  Both branches are optional.
  | IfStmt Expression (Maybe Statement) (Maybe Statement)
  -- | @case@, @casex@, or @casez@ statement.
  | CaseStmt CaseWord Expression [CaseItem]
  -- | @forever@ statement, e.g. @forever $display(\"Hello!\")@;
  | ForeverStmt Statement
  -- | @repeat@ statement, e.g. @repeat (10) \@(posedge clk);@
  | RepeatStmt Expression Statement
  -- | @while@ statement, e.g. @while (x < 10) x = x + 1;@
  | WhileStmt Expression Statement
  -- | @for@ statement, e.g. @for (i = 0; i < 10; i = i + 1) $display(\"%d\", i);@
  | ForStmt Assignment Expression Assignment Statement
  -- | @delay@ statement, e.g. @\#10 x = 1;@
  | DelayStmt Delay (Maybe Statement)
  -- | Control statement triggered by an event, e.g. @\@(posedge clk) x <= 10;@
  | EventControlStmt EventControl (Maybe Statement)
  -- | @wait@ statement, e.g. @wait (x) y <= 0;@
  | WaitStmt Expression (Maybe Statement)
  -- | a sequence of statements between @begin@ and @end@ keywords.
  -- TODO: -> <name_of_event>
  | SeqBlock (Maybe Ident) [BlockDecl] [Statement]
  -- | a set of parallel statements, enclosed between @fork@ and @join@ keywords.
  | ParBlock (Maybe Ident) [BlockDecl] [Statement]
  -- | call a task, with optional arguments like in a function call.
  | TaskStmt Ident (Maybe [Expression])
  | TaskEnableStmt Ident [Expression]
  -- TODO SystemTaskEnableStmt Ident [Expression]
  | DisableStmt Ident
  -- | @assign@ statement (like continuous assignment).
  | AssignStmt Assignment
  -- | @deassign@ statement.
  | DeAssignStmt LValue
  -- | @force@ statement.
  | ForceStmt Assignment
  -- | @release@ statement.
  | ReleaseStmt LValue
  deriving (Eq, Ord, Show, Data, Typeable)

-- | An assignment.
data Assignment
  = Assignment LValue Expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- A LValue is supposed to be limited to the following:
--    ident | ident[expr] | ident[const_expr : const_expr ] | concatentation
-- However, we don't make that restriction in our AST.
type LValue = Expression

data CaseWord
  = Case | Casex | Casez
  deriving (Eq, Ord, Data, Typeable)

instance Show CaseWord where
  show Case  = "case"
  show Casex = "casex"
  show Casez = "casez"

-- | One case item in a @case@ statement.
data CaseItem
  = CaseItem [Expression] (Maybe Statement)
  | CaseDefault (Maybe Statement)
  deriving (Eq, Ord, Show, Data, Typeable)

data BlockDecl
  = ParamDeclBlock ParamDecl
  | RegDeclBlock RegDecl
  | EventDeclBlock EventDecl
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- Miscellaneous

-- | Assign a parameter in its declaration.
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

-- | The optional timing control for a procedural assignment statement
-- (i.e. blocking and nonblocking assignments)
data AssignmentControl
  = DelayControl Delay
  | EventControl EventControl
  | RepeatControl Expression EventControl
  deriving (Eq, Ord, Show, Data, Typeable)

data EventControl
  = EventControlIdent Ident     -- ^ @\@identifier@
  | EventControlExpr EventExpr  -- ^ @\@(event_expression)@
  | EventControlWildCard        -- ^ @\@\*@
  deriving (Eq, Ord, Show, Data, Typeable)

type DelayControl = Delay

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

data ChargeStrength
  = Charge_small | Charge_medium | Charge_large
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show ChargeStrength where
  show Charge_small  = "small"
  show Charge_medium = "medium"
  show Charge_large  = "large"

data DriveStrength
  = Strength01 Strength0 Strength1
  | Strength10 Strength1 Strength0
  deriving (Eq, Ord, Show, Data, Typeable)

data Strength0
  = Supply0 | Strong0 | Pull0 | Weak0 | Highz0
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show Strength0 where
  show Supply0 = "supply0"
  show Strong0 = "strong0"
  show Pull0   = "pull0"
  show Weak0   = "weak0"
  show Highz0  = "highz0"

data Strength1
  = Supply1 | Strong1 | Pull1 | Weak1 | Highz1
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show Strength1 where
  show Supply1 = "supply1"
  show Strong1 = "strong1"
  show Pull1   = "pull1"
  show Weak1   = "weak1"
  show Highz1  = "highz1"

data NetType
  = Net_wire | Net_tri | Net_tri1 | Net_supply0
  | Net_wand | Net_triand | Net_tri0 | Net_supply1 | Net_wor
  | Net_trior | Net_triref
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show NetType where
  show Net_wire    = "wire"
  show Net_tri     = "tri"
  show Net_tri1    = "tri1"
  show Net_supply0 = "supply0"
  show Net_wand    = "wand"
  show Net_triand  = "triand"
  show Net_tri0    = "tri0"
  show Net_supply1 = "supply1"
  show Net_wor     = "wor"
  show Net_trior   = "trior"
  show Net_triref  = "triref"

data RegType
  = Reg_reg      -- ^ unsigned variable of any size = "
  | Reg_integer  -- ^ signed 32-bit variable
  | Reg_time     -- ^ unsigned 64-bit variable
  | Reg_real     -- ^ double-precision floating point variable
  | Reg_realtime -- ^ (same as above)
  deriving (Eq, Ord, Bounded, Enum, Data, Typeable)

instance Show RegType where
  show Reg_reg      = "reg"
  show Reg_integer  = "integer"
  show Reg_time     = "time"
  show Reg_real     = "real"
  show Reg_realtime = "real"

-- -----------------------------------------------------------------------------
-- GENERATED START


instance Binary Verilog where
        put (Verilog x1) = put x1
        get
          = do x1 <- get
               return (Verilog x1)


instance Binary Description where
        put x
          = case x of
                ModuleDescription x1 -> do putWord8 0
                                           put x1
                UDPDescription x1 -> do putWord8 1
                                        put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (ModuleDescription x1)
                   1 -> do x1 <- get
                           return (UDPDescription x1)
                   _ -> error "Corrupted binary data for Description"


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
                EventDeclItem x1 -> do putWord8 6
                                       put x1
                PrimitiveInstItem x1 -> do putWord8 7
                                           put x1
                ModuleInstItem x1 -> do putWord8 8
                                        put x1
                ParamOverrideItem x1 -> do putWord8 9
                                           put x1
                AssignItem x1 x2 x3 -> do putWord8 10
                                          put x1
                                          put x2
                                          put x3
                InitialItem x1 -> do putWord8 11
                                     put x1
                AlwaysItem x1 -> do putWord8 12
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
                           return (EventDeclItem x1)
                   7 -> do x1 <- get
                           return (PrimitiveInstItem x1)
                   8 -> do x1 <- get
                           return (ModuleInstItem x1)
                   9 -> do x1 <- get
                           return (ParamOverrideItem x1)
                   10 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (AssignItem x1 x2 x3)
                   11 -> do x1 <- get
                            return (InitialItem x1)
                   12 -> do x1 <- get
                            return (AlwaysItem x1)
                   _ -> error "Corrupted binary data for Item"


instance Binary UDP where
        put (UDP x1 x2 x3 x4 x5 x6)
          = do put x1
               put x2
               put x3
               put x4
               put x5
               put x6
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               x5 <- get
               x6 <- get
               return (UDP x1 x2 x3 x4 x5 x6)


instance Binary UDPDecl where
        put x
          = case x of
                UDPOutputDecl x1 -> do putWord8 0
                                       put x1
                UDPInputDecl x1 -> do putWord8 1
                                      put x1
                UDPRegDecl x1 -> do putWord8 2
                                    put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (UDPOutputDecl x1)
                   1 -> do x1 <- get
                           return (UDPInputDecl x1)
                   2 -> do x1 <- get
                           return (UDPRegDecl x1)
                   _ -> error "Corrupted binary data for UDPDecl"


instance Binary UDPInitialStatement where
        put (UDPInitialStatement x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (UDPInitialStatement x1 x2)


instance Binary TableDefinition where
        put x
          = case x of
                CombinationalTable x1 -> do putWord8 0
                                            put x1
                SequentialTable x1 -> do putWord8 1
                                         put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (CombinationalTable x1)
                   1 -> do x1 <- get
                           return (SequentialTable x1)
                   _ -> error "Corrupted binary data for TableDefinition"


instance Binary CombinationalEntry where
        put (CombinationalEntry x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (CombinationalEntry x1 x2)


instance Binary SequentialEntry where
        put (SequentialEntry x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (SequentialEntry x1 x2 x3)


instance Binary Edge where
        put x
          = case x of
                EdgeLevels x1 x2 -> do putWord8 0
                                       put x1
                                       put x2
                EdgeSymbol x1 -> do putWord8 1
                                    put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           return (EdgeLevels x1 x2)
                   1 -> do x1 <- get
                           return (EdgeSymbol x1)
                   _ -> error "Corrupted binary data for Edge"


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
        put (RegDecl x1 x2 x3)
          = do put x1
               put x2
               put x3
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               return (RegDecl x1 x2 x3)


instance Binary RegVar where
        put x
          = case x of
                RegVar x1 x2 -> do putWord8 0
                                   put x1
                                   put x2
                MemVar x1 x2 -> do putWord8 1
                                   put x1
                                   put x2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           return (RegVar x1 x2)
                   1 -> do x1 <- get
                           x2 <- get
                           return (MemVar x1 x2)
                   _ -> error "Corrupted binary data for RegVar"


instance Binary EventDecl where
        put (EventDecl x1) = put x1
        get
          = do x1 <- get
               return (EventDecl x1)


instance Binary PrimitiveInst where
        put (PrimitiveInst x1 x2 x3 x4)
          = do put x1
               put x2
               put x3
               put x4
        get
          = do x1 <- get
               x2 <- get
               x3 <- get
               x4 <- get
               return (PrimitiveInst x1 x2 x3 x4)


instance Binary PrimInst where
        put (PrimInst x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (PrimInst x1 x2)


instance Binary PrimInstName where
        put (PrimInstName x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (PrimInstName x1 x2)


instance Binary PrimType where
        put x
          = case x of
                Gate_and -> putWord8 0
                Gate_nand -> putWord8 1
                Gate_or -> putWord8 2
                Gate_nor -> putWord8 3
                Gate_xor -> putWord8 4
                Gate_xnor -> putWord8 5
                Gate_not -> putWord8 6
                Gate_buf -> putWord8 7
                Gate_bufif0 -> putWord8 8
                Gate_bufif1 -> putWord8 9
                Gate_notif0 -> putWord8 10
                Gate_notify1 -> putWord8 11
                Gate_pulldown -> putWord8 12
                Gate_pullup -> putWord8 13
                Gate_nmos -> putWord8 14
                Gate_rnmos -> putWord8 15
                Gate_pmos -> putWord8 16
                Gate_rpmos -> putWord8 17
                Gate_cmos -> putWord8 18
                Gate_rcmos -> putWord8 19
                Gate_tran -> putWord8 20
                Gate_rtran -> putWord8 21
                Gate_tranif0 -> putWord8 22
                Gate_rtranif0 -> putWord8 23
                Gate_tranif1 -> putWord8 24
                Gate_rtranif1 -> putWord8 25
        get
          = do i <- getWord8
               case i of
                   0 -> return Gate_and
                   1 -> return Gate_nand
                   2 -> return Gate_or
                   3 -> return Gate_nor
                   4 -> return Gate_xor
                   5 -> return Gate_xnor
                   6 -> return Gate_not
                   7 -> return Gate_buf
                   8 -> return Gate_bufif0
                   9 -> return Gate_bufif1
                   10 -> return Gate_notif0
                   11 -> return Gate_notify1
                   12 -> return Gate_pulldown
                   13 -> return Gate_pullup
                   14 -> return Gate_nmos
                   15 -> return Gate_rnmos
                   16 -> return Gate_pmos
                   17 -> return Gate_rpmos
                   18 -> return Gate_cmos
                   19 -> return Gate_rcmos
                   20 -> return Gate_tran
                   21 -> return Gate_rtran
                   22 -> return Gate_tranif0
                   23 -> return Gate_rtranif0
                   24 -> return Gate_tranif1
                   25 -> return Gate_rtranif1
                   _ -> error "Corrupted binary data for PrimType"


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
                CaseStmt x1 x2 x3 -> do putWord8 3
                                        put x1
                                        put x2
                                        put x3
                ForeverStmt x1 -> do putWord8 4
                                     put x1
                RepeatStmt x1 x2 -> do putWord8 5
                                       put x1
                                       put x2
                WhileStmt x1 x2 -> do putWord8 6
                                      put x1
                                      put x2
                ForStmt x1 x2 x3 x4 -> do putWord8 7
                                          put x1
                                          put x2
                                          put x3
                                          put x4
                DelayStmt x1 x2 -> do putWord8 8
                                      put x1
                                      put x2
                EventControlStmt x1 x2 -> do putWord8 9
                                             put x1
                                             put x2
                WaitStmt x1 x2 -> do putWord8 10
                                     put x1
                                     put x2
                SeqBlock x1 x2 x3 -> do putWord8 11
                                        put x1
                                        put x2
                                        put x3
                ParBlock x1 x2 x3 -> do putWord8 12
                                        put x1
                                        put x2
                                        put x3
                TaskStmt x1 x2 -> do putWord8 13
                                     put x1
                                     put x2
                TaskEnableStmt x1 x2 -> do putWord8 14
                                           put x1
                                           put x2
                DisableStmt x1 -> do putWord8 15
                                     put x1
                AssignStmt x1 -> do putWord8 16
                                    put x1
                DeAssignStmt x1 -> do putWord8 17
                                      put x1
                ForceStmt x1 -> do putWord8 18
                                   put x1
                ReleaseStmt x1 -> do putWord8 19
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
                           x2 <- get
                           x3 <- get
                           return (CaseStmt x1 x2 x3)
                   4 -> do x1 <- get
                           return (ForeverStmt x1)
                   5 -> do x1 <- get
                           x2 <- get
                           return (RepeatStmt x1 x2)
                   6 -> do x1 <- get
                           x2 <- get
                           return (WhileStmt x1 x2)
                   7 -> do x1 <- get
                           x2 <- get
                           x3 <- get
                           x4 <- get
                           return (ForStmt x1 x2 x3 x4)
                   8 -> do x1 <- get
                           x2 <- get
                           return (DelayStmt x1 x2)
                   9 -> do x1 <- get
                           x2 <- get
                           return (EventControlStmt x1 x2)
                   10 -> do x1 <- get
                            x2 <- get
                            return (WaitStmt x1 x2)
                   11 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (SeqBlock x1 x2 x3)
                   12 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (ParBlock x1 x2 x3)
                   13 -> do x1 <- get
                            x2 <- get
                            return (TaskStmt x1 x2)
                   14 -> do x1 <- get
                            x2 <- get
                            return (TaskEnableStmt x1 x2)
                   15 -> do x1 <- get
                            return (DisableStmt x1)
                   16 -> do x1 <- get
                            return (AssignStmt x1)
                   17 -> do x1 <- get
                            return (DeAssignStmt x1)
                   18 -> do x1 <- get
                            return (ForceStmt x1)
                   19 -> do x1 <- get
                            return (ReleaseStmt x1)
                   _ -> error "Corrupted binary data for Statement"


instance Binary Assignment where
        put (Assignment x1 x2)
          = do put x1
               put x2
        get
          = do x1 <- get
               x2 <- get
               return (Assignment x1 x2)


instance Binary CaseWord where
        put x
          = case x of
                Case -> putWord8 0
                Casex -> putWord8 1
                Casez -> putWord8 2
        get
          = do i <- getWord8
               case i of
                   0 -> return Case
                   1 -> return Casex
                   2 -> return Casez
                   _ -> error "Corrupted binary data for CaseWord"


instance Binary CaseItem where
        put x
          = case x of
                CaseItem x1 x2 -> do putWord8 0
                                     put x1
                                     put x2
                CaseDefault x1 -> do putWord8 1
                                     put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           x2 <- get
                           return (CaseItem x1 x2)
                   1 -> do x1 <- get
                           return (CaseDefault x1)
                   _ -> error "Corrupted binary data for CaseItem"


instance Binary BlockDecl where
        put x
          = case x of
                ParamDeclBlock x1 -> do putWord8 0
                                        put x1
                RegDeclBlock x1 -> do putWord8 1
                                      put x1
                EventDeclBlock x1 -> do putWord8 2
                                        put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (ParamDeclBlock x1)
                   1 -> do x1 <- get
                           return (RegDeclBlock x1)
                   2 -> do x1 <- get
                           return (EventDeclBlock x1)
                   _ -> error "Corrupted binary data for BlockDecl"


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


instance Binary AssignmentControl where
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
                   _ -> error "Corrupted binary data for AssignmentControl"


instance Binary EventControl where
        put x
          = case x of
                EventControlIdent x1 -> do putWord8 0
                                           put x1
                EventControlExpr x1 -> do putWord8 1
                                          put x1
                EventControlWildCard -> putWord8 2
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (EventControlIdent x1)
                   1 -> do x1 <- get
                           return (EventControlExpr x1)
                   2 -> return EventControlWildCard
                   _ -> error "Corrupted binary data for EventControl"


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


instance Binary ChargeStrength where
        put x
          = case x of
                Charge_small -> putWord8 0
                Charge_medium -> putWord8 1
                Charge_large -> putWord8 2
        get
          = do i <- getWord8
               case i of
                   0 -> return Charge_small
                   1 -> return Charge_medium
                   2 -> return Charge_large
                   _ -> error "Corrupted binary data for ChargeStrength"


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


instance Binary Strength0 where
        put x
          = case x of
                Supply0 -> putWord8 0
                Strong0 -> putWord8 1
                Pull0 -> putWord8 2
                Weak0 -> putWord8 3
                Highz0 -> putWord8 4
        get
          = do i <- getWord8
               case i of
                   0 -> return Supply0
                   1 -> return Strong0
                   2 -> return Pull0
                   3 -> return Weak0
                   4 -> return Highz0
                   _ -> error "Corrupted binary data for Strength0"


instance Binary Strength1 where
        put x
          = case x of
                Supply1 -> putWord8 0
                Strong1 -> putWord8 1
                Pull1 -> putWord8 2
                Weak1 -> putWord8 3
                Highz1 -> putWord8 4
        get
          = do i <- getWord8
               case i of
                   0 -> return Supply1
                   1 -> return Strong1
                   2 -> return Pull1
                   3 -> return Weak1
                   4 -> return Highz1
                   _ -> error "Corrupted binary data for Strength1"


instance Binary NetType where
        put x
          = case x of
                Net_wire -> putWord8 0
                Net_tri -> putWord8 1
                Net_tri1 -> putWord8 2
                Net_supply0 -> putWord8 3
                Net_wand -> putWord8 4
                Net_triand -> putWord8 5
                Net_tri0 -> putWord8 6
                Net_supply1 -> putWord8 7
                Net_wor -> putWord8 8
                Net_trior -> putWord8 9
                Net_triref -> putWord8 10
        get
          = do i <- getWord8
               case i of
                   0 -> return Net_wire
                   1 -> return Net_tri
                   2 -> return Net_tri1
                   3 -> return Net_supply0
                   4 -> return Net_wand
                   5 -> return Net_triand
                   6 -> return Net_tri0
                   7 -> return Net_supply1
                   8 -> return Net_wor
                   9 -> return Net_trior
                   10 -> return Net_triref
                   _ -> error "Corrupted binary data for NetType"


instance Binary RegType where
        put x
          = case x of
                Reg_reg -> putWord8 0
                Reg_integer -> putWord8 1
                Reg_time -> putWord8 2
                Reg_real -> putWord8 3
                Reg_realtime -> putWord8 4
        get
          = do i <- getWord8
               case i of
                   0 -> return Reg_reg
                   1 -> return Reg_integer
                   2 -> return Reg_time
                   3 -> return Reg_real
                   4 -> return Reg_realtime
                   _ -> error "Corrupted binary data for RegType"
-- GENERATED STOP
