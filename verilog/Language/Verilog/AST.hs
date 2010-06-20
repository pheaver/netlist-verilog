--------------------------------------------------------------------------------
-- |
-- Module       :  Language.Verilog.AST
-- Copyright    :  (c) Signali Corp. 2010
-- License      :  All rights reserved
--
-- Maintainer   : pweaver@signalicorp.com
-- Stability    : experimental
-- Portability  : non-portable (DeriveDataTypeable)
--
-- An abstract syntax tree (AST) for Verilog, derived form
-- <http://www.verilog.com/VerilogBNF.html>, and extended to support some
-- Verilog-2001 constructs.  The URL above is referred to as the \"spec\"
-- throughout the documentaiton.
--
-- See also the Wikipedia article on Verilog:
-- <http://en.wikipedia.org/wiki/Verilog>.
--
-- This AST is very close to the concrete syntax, and is only meant for
-- generating, pretty printing, and parsing.  To do anything more advanced, it
-- should be converted into another form.
--
-- The AST is incomplete in many places, and deviates from the spec in a few
-- places, too.
--------------------------------------------------------------------------------

{-# LANGUAGE DeriveDataTypeable, TypeOperators #-}
{-# OPTIONS_DERIVE --append -d Binary #-}

module Language.Verilog.AST
  (
  -- * The top-level types
  Verilog(..), Module(..), Description(..),

  -- * User-defined primitives
  UDP(..), UDPDecl(..), UDPInitialStatement(..),
  TableDefinition(..), CombinationalEntry(..), SequentialEntry(..),
  LevelSymbol, levelSymbols, validLevelSymbol,
  OutputSymbol, outputSymbols, validOutputSymbol,
  NextState, nextStates, validNextState,
  InputList(..), Edge(..), EdgeSymbol, edgeSymbols, validEdgeSymbol,

  -- * Items and Declarations
  Item(..), ParamDecl(..), InputDecl(..), OutputDecl(..), InOutDecl(..),
  NetDecl(..), RegDecl(..), TimeDecl(..), IntegerDecl(..), RealDecl(..), EventDecl(..),

  -- * Primitive Instances

  -- * Module Instantiations
  ModuleInst(..), Parameter(..), Instance(..), Connections(..), NamedConnection(..),

  -- * Behavioral Statements
  Statement(..), Assignment(..), CaseItem(..), BlockDecl(..),

  -- * Expressions
  Expression(..), ConstExpr, LValue, UnaryOp(..), BinaryOp(..),

  -- * Miscellaneous
  Ident(..), ParamAssign(..), ExpandRange(..), Range(..), RegVar(..),
  DelayOrEventControl(..), DelayControl, EventControl, Delay,
  EventExpr(..), ScalarEventExpr,
  ChargeStrength, charge_strengths, DriveStrength(..),
  Strength0, strength0s, Strength1, strength1s, NetType, net_types
  ) where

import Data.Binary      ( Binary(..), putWord8, getWord8 )
import Data.Generics    ( Data, Typeable )

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
  | TimeDeclItem TimeDecl
  | IntegerDeclItem IntegerDecl
  | RealDeclItem RealDecl
  | EventDeclItem EventDecl
  -- TODO: GateDecl
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
  | UDPRegDecl RegDecl
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
  = SequentialEntry InputList LevelSymbol NextState
  deriving (Eq, Ord, Show, Data, Typeable)

data InputList
  = LevelInputList [LevelSymbol]
  | EdgeInputList {-[LevelSymbol]-} Edge {-[LevelSymbol]-}
  -- the spec says
  --    <edge_input_list> ::= <LEVEL_SYMBOL>* <edge> <LEVEL_SYMBOL>*
  -- but I haven't figured out what the LEVEL_SYMBOLs are for yet,
  -- so I left them out.
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

{-
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
-}

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
  = BlockingAssignment LValue (Maybe DelayOrEventControl) Expression
  -- | non-blocking assignment, e.g. @x \<= y@
  | NonBlockingAssignment LValue (Maybe DelayOrEventControl) Expression
  -- | @if@ statement.  Both branches are optional.
  | IfStmt Expression (Maybe Statement) (Maybe Statement)
  -- | @case@ statement.
  | CaseStmt Expression [CaseItem]
  -- TODO: casex and casez
  -- TODO: CaseStmt Expression [CaseItem]
  -- | @forever@ statement, e.g. @forever $display(\"Hello!\")@;
  | ForeverStmt Statement
  -- | @repeat@ statement, e.g. @repeat (10) \@(posedge clk);@
  | RepeatStmt Expression Statement
  -- | @while@ statement, e.g. @while (x < 10) x = x + 1;@
  | WhileStmt Expression Statement
  -- | @for@ statement, e.g. @for (i = 0; i < 10; i = i + 1) $display(\"%d\", i);@
  | ForStmt Assignment Expression Assignment Statement
  -- | delay or event control statement, e.g. @\@(posedge clk) x <= 10;@, or @#10 x <= 10;@
  | DelayOrEventControlStmt DelayOrEventControl (Maybe Statement)
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

-- | One case item in a @case@ statement.
data CaseItem
  = CaseItem [Expression] (Maybe Statement)
  | CaseDefault (Maybe Statement)
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

type ConstExpr = Expression

-- data Base = BinBase | OctBase | DecBase | HexBase
--   deriving (Eq, Ord, Show, Data, Typeable)

-- A LValue is supposed to be limited to the following:
--    ident | ident[expr] | ident[const_expr : const_expr ] | concatentation
-- However, we don't make that restriction in our AST.
type LValue = Expression

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
  deriving (Eq, Ord, Show, Data, Typeable)

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
  deriving (Eq, Ord, Show, Data, Typeable)

-- -----------------------------------------------------------------------------
-- Miscellaneous

newtype Ident = Ident String
  deriving (Eq, Ord, Show, Data, Typeable)

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
                ParamOverrideItem x1 -> do putWord8 11
                                           put x1
                AssignItem x1 x2 x3 -> do putWord8 12
                                          put x1
                                          put x2
                                          put x3
                InitialItem x1 -> do putWord8 13
                                     put x1
                AlwaysItem x1 -> do putWord8 14
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
                            return (ParamOverrideItem x1)
                   12 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (AssignItem x1 x2 x3)
                   13 -> do x1 <- get
                            return (InitialItem x1)
                   14 -> do x1 <- get
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


instance Binary InputList where
        put x
          = case x of
                LevelInputList x1 -> do putWord8 0
                                        put x1
                EdgeInputList x1 -> do putWord8 1
                                       put x1
        get
          = do i <- getWord8
               case i of
                   0 -> do x1 <- get
                           return (LevelInputList x1)
                   1 -> do x1 <- get
                           return (EdgeInputList x1)
                   _ -> error "Corrupted binary data for InputList"


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
                CaseStmt x1 x2 -> do putWord8 3
                                     put x1
                                     put x2
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
                DelayOrEventControlStmt x1 x2 -> do putWord8 8
                                                    put x1
                                                    put x2
                WaitStmt x1 x2 -> do putWord8 9
                                     put x1
                                     put x2
                SeqBlock x1 x2 x3 -> do putWord8 10
                                        put x1
                                        put x2
                                        put x3
                ParBlock x1 x2 x3 -> do putWord8 11
                                        put x1
                                        put x2
                                        put x3
                TaskStmt x1 x2 -> do putWord8 12
                                     put x1
                                     put x2
                TaskEnableStmt x1 x2 -> do putWord8 13
                                           put x1
                                           put x2
                DisableStmt x1 -> do putWord8 14
                                     put x1
                AssignStmt x1 -> do putWord8 15
                                    put x1
                DeAssignStmt x1 -> do putWord8 16
                                      put x1
                ForceStmt x1 -> do putWord8 17
                                   put x1
                ReleaseStmt x1 -> do putWord8 18
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
                           return (CaseStmt x1 x2)
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
                           return (DelayOrEventControlStmt x1 x2)
                   9 -> do x1 <- get
                           x2 <- get
                           return (WaitStmt x1 x2)
                   10 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (SeqBlock x1 x2 x3)
                   11 -> do x1 <- get
                            x2 <- get
                            x3 <- get
                            return (ParBlock x1 x2 x3)
                   12 -> do x1 <- get
                            x2 <- get
                            return (TaskStmt x1 x2)
                   13 -> do x1 <- get
                            x2 <- get
                            return (TaskEnableStmt x1 x2)
                   14 -> do x1 <- get
                            return (DisableStmt x1)
                   15 -> do x1 <- get
                            return (AssignStmt x1)
                   16 -> do x1 <- get
                            return (DeAssignStmt x1)
                   17 -> do x1 <- get
                            return (ForceStmt x1)
                   18 -> do x1 <- get
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


instance Binary Ident where
        put (Ident x1) = put x1
        get
          = do x1 <- get
               return (Ident x1)


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
